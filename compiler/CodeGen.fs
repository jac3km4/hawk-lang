module CodeGen

open System
open System.Reflection
open System.Reflection.Emit
open System.Collections.Generic
open IR

let typeDefOf = 
    function 
    | Type.Int -> typedefof<int>
    | Type.Float -> typedefof<float>
    | Type.String -> typedefof<string>
    | Type.Bool -> typedefof<bool>
    | Type.Unit -> typedefof<unit>
    | Type.Tuple(_) -> failwith "Not implemented yet"
    | Type.Fun(_, _) -> failwith "Not implemented yet"
    | _ -> failwith "Unexpected type"

type Symbol = 
    | Method of MethodInfo

type Env = 
    { tb : TypeBuilder
      ilg : ILGenerator
      symbols : Map<string, Symbol> }

let rec codegen ({ tb = tb; ilg = ilg; symbols = sym } as env) ir : Env = 
    match ir with
    | Decfun(id, ret, args, body) -> 
        let fn = 
            tb.DefineMethod
                (id, MethodAttributes.Public ||| MethodAttributes.Static, typeDefOf ret, 
                 Seq.map typeDefOf args |> Seq.toArray)
        let env = codegen { env with ilg = fn.GetILGenerator() } body
        { env with symbols = Map.add id (Method fn) env.symbols }
    | Call(id, args) -> 
        List.iter (fun arg -> codegen env arg |> ignore) args
        let mi = 
            match sym.Item id with
            | Method m -> m
            | _ -> failwith "Not found function"
        ilg.EmitCall(OpCodes.Call, mi, null)
        env
    | Decloc(tpe) -> 
        ilg.DeclareLocal(typeDefOf tpe) |> ignore
        env
    | Stloc(idx, ir) -> 
        codegen env ir |> ignore
        ilg.Emit(OpCodes.Stloc, idx)
        env
    | Ldloc idx -> 
        ilg.Emit(OpCodes.Ldloc, idx)
        env
    | Ldarg idx -> 
        ilg.Emit(OpCodes.Ldarg, idx)
        env
    | Const c -> 
        match c with
        | IConst i -> ilg.Emit(OpCodes.Ldc_I4, i)
        | FConst f -> ilg.Emit(OpCodes.Ldc_R4, f)
        | SConst s -> ilg.Emit(OpCodes.Ldstr, s)
        env
    | UnOp(op, ir) -> 
        codegen env ir |> ignore
        match op with
        | Neg -> ilg.Emit(OpCodes.Neg)
        | Not -> ilg.Emit(OpCodes.Not)
        env
    | BinOp(l, op, r) -> 
        codegen env l |> ignore
        codegen env r |> ignore
        match op with
        | Add -> ilg.Emit(OpCodes.Add)
        | Sub -> ilg.Emit(OpCodes.Sub)
        | Mul -> ilg.Emit(OpCodes.Mul)
        | Div -> ilg.Emit(OpCodes.Div)
        | Eq -> ilg.Emit(OpCodes.Ceq)
        | Mod -> failwith "Not implemented yet"
        | Ge -> failwith "Not implemented yet"
        | Le -> failwith "Not implemented yet"
        | Greater -> ilg.Emit(OpCodes.Cgt)
        | Less -> ilg.Emit(OpCodes.Clt)
        | StringConcat -> 
            let concat = 
                typedefof<String>.GetMethod("Concat", 
                                            [| typedefof<string>
                                               typedefof<string> |])
            ilg.Emit(OpCodes.Call, concat)
        | StringEqual -> 
            let equals = 
                typedefof<String>.GetMethod("Equals", 
                                            [| typedefof<string>
                                               typedefof<string> |])
            ilg.Emit(OpCodes.Call, equals)
        env
    | BranchTrueFalse(l, r, ifB, elseB) -> 
        codegen env l |> ignore
        codegen env r |> ignore
        let ifL = ilg.DefineLabel()
        let elseL = ilg.DefineLabel()
        let out = ilg.DefineLabel()
        ilg.Emit(OpCodes.Beq, ifL)
        ilg.Emit(OpCodes.Br, elseL)
        ilg.MarkLabel(ifL)
        codegen env ifB |> ignore
        ilg.Emit(OpCodes.Br, out)
        ilg.MarkLabel(elseL)
        codegen env elseB |> ignore
        ilg.MarkLabel(out)
        env
    | Compound code -> List.fold (fun st ir -> codegen st ir) env code
    | Conv(ir, target) -> 
        //TODO this needs a fix
        codegen env ir |> ignore
        match target with
        | Type.Float -> ilg.Emit(OpCodes.Conv_R4)
        | Type.Int -> ilg.Emit(OpCodes.Conv_I4)
        | _ -> failwith "Unexpected type"
        env
    | Ret ir -> 
        codegen env ir |> ignore
        ilg.Emit(OpCodes.Ret)
        env
    | _ -> env

let assembly name tree = 
    printfn "IR Tree:\n%A" tree
    let asm = AppDomain.CurrentDomain.DefineDynamicAssembly(new AssemblyName(name), AssemblyBuilderAccess.RunAndSave)
    let mb = asm.DefineDynamicModule(name + ".exe")
    let prog = mb.DefineType(name, TypeAttributes.Class)
    let main = prog.DefineMethod("Main", MethodAttributes.Public ||| MethodAttributes.Static)
    codegen { tb = prog
              ilg = main.GetILGenerator()
              symbols = Map.empty } tree
    |> ignore
    main.GetILGenerator().Emit OpCodes.Ret
    prog.CreateType() |> ignore
    asm.SetEntryPoint main
    asm
