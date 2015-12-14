module IR

type Label = string

type Const = 
    | IConst of int
    | FConst of float
    | SConst of string

type BinOp = 
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | Eq
    | Ge
    | Le
    | Greater
    | Less
    | StringConcat
    | StringEqual

type UnOp = 
    | Neg
    | Not

type IR = 
    | BinOp of IR * BinOp * IR
    | UnOp of UnOp * IR
    | Decloc of Type.Type
    | Decfun of Label * Type.Type * Type.Type list * IR
    | Funvar of Label * Type.Type * Type.Type list
    | Call of Label * IR list
    | Stloc of int * IR
    | Ldloc of int
    | Ldarg of int
    | Const of Const
    | Ret of IR
    | Compound of IR list
    | BranchTrueFalse of IR * IR * IR * IR
    | Branch of IR
    | Conv of IR * Type.Type
    | Nop
