module Common

open System.Collections.Generic

exception CompilationError of string

let astOpToIR = 
    function 
    | Ast.Add -> IR.Add
    | Ast.Sub -> IR.Sub
    | Ast.Mul -> IR.Mul
    | Ast.Div -> IR.Div
    | Ast.Mod -> IR.Mod
    | Ast.Eq -> IR.Eq
    | Ast.Ge -> IR.Ge
    | Ast.Le -> IR.Le
    | Ast.Greater -> IR.Greater
    | Ast.Less -> IR.Less

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
