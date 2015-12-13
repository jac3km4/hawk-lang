module Compilation

open System
open IR
open System.Collections.Generic

type Ref = 
    { idx : int
      tpe : Type.Type }

type Val = 
    | Arg of Ref
    | Loc of Ref

type Def = 
    { id : string
      ret : Type.Type
      args : Type.Type list }

type Var = 
    | Def of Def
    | Val of Val

type FunContext = 
    { args : Ref list
      locals : Ref list }

type ScopeContext = 
    { vals : Map<string, Val>
      defs : Map<string, Def> }

type Env = 
    { scopes : Stack<ScopeContext>
      fn : Stack<FunContext> }

exception CompilationError of string

//TODO implement type unifying rules
let rec uni ir tpe target = 
    match (target, tpe) with
    | (Type.Int, Type.Int) | (Type.Float, Type.Float) | (Type.Bool, Type.Bool) | (Type.Bool, Type.Int) | (Type.Int, 
                                                                                                          Type.Bool) -> 
        ir
    | _ -> failwith "Not implemented yet"

let rec run (env : Env) e = 
    match e with
    | Ast.Literal l -> 
        match l with
        | Ast.Unit -> Nop, Type.Unit
        | Ast.Int i -> Const(IConst i), Type.Int
        | Ast.Float f -> Const(FConst f), Type.Float
        | Ast.String s -> Const(SConst s), Type.String
        | Ast.Bool b -> 
            Const(IConst(if b then 1
                         else 0)), Type.Bool
    | Ast.Block exprs -> 
        let scope = 
            { vals = Map.empty
              defs = Map.empty }
        env.scopes.Push scope
        let rev = List.rev exprs
        let ops = List.map (fun x -> run env x |> fst) (List.tail rev |> List.rev)
        let (rop, rtpe) = run env (List.head rev)
        env.scopes.Pop |> ignore
        Compound(List.append ops [ rop ]), rtpe
    | Ast.Var id -> 
        //TODO closure handling is broken
        let var = 
            seq { 
                for scope in env.scopes do
                    match scope.vals.TryFind id with
                    | Some v -> yield Val v
                    | None -> ()
                    match scope.defs.TryFind id with
                    | Some v -> yield Def v
                    | None -> ()
            }
            |> Seq.tryHead
        match var with
        | Some(Val(Loc t)) -> Ldloc t.idx, t.tpe
        | Some(Val(Arg a)) -> Ldarg a.idx, a.tpe
        | Some(Def def) -> Funvar(def.id, def.ret, def.args), Type.Fun(def.args, def.ret)
        | None -> raise (CompilationError(sprintf "Unknown identifier: %s" id))
    | Ast.UnOp(op, ex) -> 
        let (ir, t) = run env ex
        match op with
        | Ast.Neg -> 
            match t with
            | Type.Int | Type.Float -> (UnOp(Neg, ir)), t
            | _ -> raise (CompilationError("Invalid type"))
        | Ast.Not -> 
            let cir = uni ir t Type.Bool
            UnOp(Not, cir), Type.Bool
    | Ast.BinOp(le, op, re) -> 
        //TODO implement binary ops properly...
        let (lir, lt) = run env le
        let (rir, rt) = run env re
        
        let irOp = 
            match op with
            | Ast.Add -> Add
            | Ast.Sub -> Sub
            | Ast.Mul -> Mul
            | Ast.Div -> Div
            | Ast.Eq -> Eq
        
        let cl = uni lir lt rt
        BinOp(cl, irOp, rir), rt
    | Ast.IfBranch(cond, ifBranch, elseBranch) -> 
        let (condIr, condType) = run env cond
        let cCondIr = uni condIr condType Type.Bool
        let (ifIr, ifType) = run env ifBranch
        
        let elseIr = 
            match elseBranch with
            | Some branch -> 
                let (tir, tt) = run env branch
                uni tir tt ifType
            | None -> Nop
        BranchTrueFalse(cCondIr, Const(IConst 1), ifIr, elseIr), ifType
    | Ast.Val(id, typeAn, ex) -> 
        let (ir, exType) = run env ex
        let scope = env.scopes.Pop()
        let fn = env.fn.Pop()
        let idx = fn.locals.Length
        
        let (cir, ct) = 
            match typeAn with
            | Some tpe -> uni ir exType tpe, tpe
            | None -> ir, exType
        
        let local = 
            { idx = idx
              tpe = ct }
        
        env.scopes.Push { vals = Map.add id (Loc local) scope.vals
                          defs = scope.defs }
        env.fn.Push { locals = local :: fn.locals
                      args = fn.args }
        Compound [ Decloc ct
                   Stloc(idx, cir) ], Type.Unit
    | Ast.ValTuple(ids, ex) -> failwith "Not implemented yet"
    | Ast.Def(id, typeAn, args, body) -> 
        if not (List.forall (fun (_, t) -> Option.isSome t) args) then 
            raise (CompilationError(sprintf "All arguments must have types specified, in \"%s\" declaration" id))
        let argRefs = 
            List.indexed args |> List.map (fun (idx, (_, t)) -> 
                                     { idx = idx
                                       tpe = Option.get t })
        
        let outer = env.scopes.Pop()
        let inner = 
            List.append (List.zip (List.map (fun (i, _) -> i) args) (List.map (fun x -> Arg x) argRefs)) 
                (Map.toList outer.vals) |> Map.ofList
        env.scopes.Push { vals = inner
                          defs = outer.defs }
        env.fn.Push { locals = List.empty
                      args = argRefs }
        let (ir, exType) = run env body
        
        let (cir, ct) = 
            match typeAn with
            | Some tpe -> uni ir exType tpe, tpe
            | None -> ir, exType
        
        let irWithRet = 
            match cir with
            | Compound ops -> 
                let rev = List.rev ops
                Compound(List.rev (Ret(List.head rev) :: (List.tail rev)))
            | o -> Ret o
        
        env.fn.Pop() |> ignore
        env.scopes.Pop() |> ignore
        let argt = List.map (fun (_, t) -> Option.get t) args
        env.scopes.Push { vals = outer.vals
                          defs = 
                              Map.add id { id = id
                                           ret = ct
                                           args = argt } outer.defs }
        Decfun(id, ct, argt, irWithRet), Type.Unit
    | Ast.Apply(f, args) -> 
        match run env f with
        | Funvar(id, rt, argt), _ -> 
            if not (args.Length = argt.Length) then 
                raise 
                    (CompilationError
                         (sprintf "Invalid number of arguments for \"%s\", %d expected, got %d" id argt.Length 
                              args.Length))
            let argsIr = 
                List.zip args argt |> List.map (fun (arg, expected) -> 
                                          let (ir, t) = run env arg
                                          uni ir t expected)
            Call(id, argsIr), rt
        | t -> raise (CompilationError(sprintf "Expecting function, got %A" (snd t)))

let normalize ast = 
    printfn "Ast:\n%A" ast
    let scopes = new Stack<ScopeContext>()
    let fns = new Stack<FunContext>()
    fns.Push { locals = List.empty
               args = List.empty }
    scopes.Push { vals = Map.empty
                  defs = Map.empty }
    run { scopes = scopes
          fn = fns } ast
    |> fst


