
#r "packages/FsCheck.2.2.3/lib/net45/FsCheck.dll"
#r "bin/Debug/falcon_lang.dll"

open FsCheck

type Tests() = 
    static member ``compilation throws on invalid var name`` = 
        Prop.throws<Compilation.CompilationError, _> (Lazy.Create(fun () ->
            let code = @"{
                    val x = 100
                    x + a
                }"
            Compilation.compile code "Test"
        ))

Check.All<Tests> {Config.Default with MaxTest = 1}
