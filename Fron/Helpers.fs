module Helpers

open CommonTypes

let tryCreateLimited (toValue: int) (ctor: int -> 'a) (value: int) : ExpressionErrorResult<'a> =
    if value >= 0 && value < toValue then
        value |> ctor |> Ok
    else
        Error(ExpressionError $"{value} is beyond permitted range.")

let tryCreateLessThanSixty ctor (value: int) = value |> tryCreateLimited 60 ctor
