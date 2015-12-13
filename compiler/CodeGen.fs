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

let rec codegen (tpe : TypeBuilder) (funs : Dictionary<string, MethodInfo>) (gen : ILGenerator) ir : unit = 
    match ir with
    | Decfun(id, ret, args, body) -> 
        let fn = 
            tpe.DefineMethod
                (id, MethodAttributes.Public ||| MethodAttributes.Static, typeDefOf ret, 
                 Seq.map typeDefOf args |> Seq.toArray)
        funs.Add(id, fn)
        codegen tpe funs (fn.GetILGenerator()) body
    | Call(id, args) -> 
        List.iter (codegen tpe funs gen) args
        gen.EmitCall(OpCodes.Call, funs.Item id, null)
    | Decloc(tpe) -> gen.DeclareLocal(typeDefOf tpe) |> ignore
    | Stloc(idx, ops) -> 
        codegen tpe funs gen ops
        gen.Emit(OpCodes.Stloc, idx)
    | Ldloc idx -> gen.Emit(OpCodes.Ldloc, idx)
    | Ldarg idx -> gen.Emit(OpCodes.Ldarg, idx)
    | Const c -> 
        match c with
        | IConst i -> gen.Emit(OpCodes.Ldc_I4, i)
        | FConst f -> gen.Emit(OpCodes.Ldc_R4, f)
        | SConst s -> gen.Emit(OpCodes.Ldstr, s)
    | BinOp(l, op, r) -> 
        codegen tpe funs gen l
        codegen tpe funs gen r
        let opcode = 
            match op with
            | Add -> OpCodes.Add
            | Sub -> OpCodes.Sub
            | Mul -> OpCodes.Mul
            | Div -> OpCodes.Div
            | Eq -> OpCodes.Ceq
        gen.Emit(opcode)
    | BranchTrueFalse(l, r, ifB, elseB) -> 
        codegen tpe funs gen l
        codegen tpe funs gen r
        let ifL = gen.DefineLabel()
        let elseL = gen.DefineLabel()
        let out = gen.DefineLabel()
        gen.Emit(OpCodes.Beq, ifL)
        gen.Emit(OpCodes.Br, elseL)
        gen.MarkLabel(ifL)
        codegen tpe funs gen ifB
        gen.Emit(OpCodes.Br, out)
        gen.MarkLabel(elseL)
        codegen tpe funs gen elseB
        gen.MarkLabel(out)
    | Compound ops -> List.iter (codegen tpe funs gen) ops
    | Ret op -> 
        codegen tpe funs gen op
        gen.Emit(OpCodes.Ret)
    | _ -> ()

let assembly name tree = 
    printfn "IR Tree:\n%A" tree
    let asm = AppDomain.CurrentDomain.DefineDynamicAssembly(new AssemblyName(name), AssemblyBuilderAccess.RunAndSave)
    let mb = asm.DefineDynamicModule(name + ".exe")
    let prog = mb.DefineType(name, TypeAttributes.Class)
    let main = prog.DefineMethod("Main", MethodAttributes.Public ||| MethodAttributes.Static)
    codegen prog (new Dictionary<string, MethodInfo>()) (main.GetILGenerator()) tree
    main.GetILGenerator().Emit OpCodes.Ret
    prog.CreateType() |> ignore
    asm.SetEntryPoint main
    asm
