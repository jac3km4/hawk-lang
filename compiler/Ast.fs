module Ast

type Id = string

type Expr = 
    | Literal of Literal
    | Block of Expr list
    | Var of Id
    | UnOp of UnOp * Expr
    | BinOp of Expr * BinOp * Expr
    | Val of Id * Type.Type option * Expr
    | ValTuple of (Id * Type.Type option) list * Expr
    | Def of Id * Type.Type option * (Id * Type.Type option) list * Expr
    | Apply of Expr * Expr list
    | IfBranch of Expr * Expr * Expr option

and Literal = 
    | Unit
    | Int of int
    | Float of float
    | String of string
    | Bool of bool

and UnOp = 
    | Neg
    | Not

and BinOp = 
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | Eq
    | Le
    | Ge
    | Greater
    | Less
