module Type

type Type = 
    | Any
    | Unit
    | Int
    | Float
    | Bool
    | String
    | Tuple of Type list
    | Fun of Type list * Type
