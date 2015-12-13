module Common

open System.Collections.Generic

exception CompilationError of string

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
