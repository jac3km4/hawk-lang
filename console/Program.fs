module Program

open Util

type Options = 
    { output : string }

let rec parseArgs args options = 
    match args with
    | [] -> options
    | "-o" :: path :: xs -> parseArgs xs { options with output = path }
    | other :: xs -> parseArgs xs options

[<EntryPoint>]
let main args = 
    if args.Length > 0 then 
        let opts = parseArgs (Seq.skip 1 args |> Seq.toList) { output = "out.exe" }
        let code = System.IO.File.ReadAllText args.[0]
        try 
            let asm = compile code "Program"
            asm.Save opts.output
        with Common.CompilationError err -> printfn "Compilation error:\n%s" err
    else printfn "Usage: source [-o output]"
    0
