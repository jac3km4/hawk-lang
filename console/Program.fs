module Program

open Util

[<EntryPoint>]
let main args = 
    if args.Length > 1 then 
        let code = System.IO.File.ReadAllText args.[1]
        try 
            let asm = compile code "Program"
            asm.Save("out.exe")
        with Compilation.CompilationError err -> printfn "Compilation error:\n%s" err
    else printfn "Expecting source file as an argument"
    0
