module Fron.Helpers

open Types

let tryCreateLimited (fromValue: int) (toValue: int) (ctor: int -> 'a) (value: int) : ExpressionErrorResult<'a> =
    if value >= fromValue && value < toValue then
        value |> ctor |> Ok
    else
        Error(ExpressionError $"{value} is beyond permitted range.")

let tryCreateLimitedPositive (toValue: int) (ctor: int -> 'a) (value: int) : ExpressionErrorResult<'a> =
    tryCreateLimited 0 toValue ctor value