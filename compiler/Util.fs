module Util

open Microsoft.FSharp.Text.Lexing

let compile code name = 
    LexBuffer<char>.FromString code
    |> Parser.start Lexer.token
    |> Compilation.normalize
    |> CodeGen.assembly name
