module Fron.Helpers

open Fron.CommonTypes

let (|BetweenInclusive|_|) lo hi x =
    if lo <= x && x <= hi then
        Some()
    else
        None

let tryCreateLimited
    (fromValue: int)
    (toValue: int)
    (ctor: int -> 'a)
    (onError: int -> ExpressionError)
    (value: int)
    : Result<'a, ExpressionError> =
    match value with
    | BetweenInclusive fromValue toValue -> value |> ctor |> Ok
    | _ -> Error(onError value)
