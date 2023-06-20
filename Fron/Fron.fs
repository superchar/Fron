module Fron

open Fron.Helpers
open Fron.CommonTypes
open Microsoft.FSharp.Core

type CronExpression = CronExpression of string

type private CronExpressionParameters =
    { Seconds: int
      Minutes: int
      Hours: int }

type CronExpressionElement =
    | Seconds of int
    | Minutes of int
    | Hours of int

type BuildExpressionResult = Result<CronExpressionElement list, ExpressionError>
type ExpressionResult = Result<CronExpression, ExpressionError>

type NewCronExpression = NewCronExpression of int * BuildExpressionResult

let private createRangeError value =
    ExpressionError $"{value} is beyond permitted range."

let private createZeroSixtyUnit ctor (NewCronExpression (value, current)) =
    current
    |> Result.bind (fun list ->
        value
        |> tryCreateLimited 0 60 ctor createRangeError
        |> Result.map (fun item -> item :: list))

let triggerEvery (value: int) : NewCronExpression = NewCronExpression(value, Ok [])

let andAlso (value: int) (current: BuildExpressionResult) = NewCronExpression(value, current)

let seconds = createZeroSixtyUnit Seconds

let minutes = createZeroSixtyUnit Minutes

let hours = createZeroSixtyUnit Hours

let generate (current: BuildExpressionResult) : ExpressionResult =
    let getParameters (elements: CronExpressionElement list) : CronExpressionParameters =
        elements
        |> List.fold
            (fun acc item ->
                match item with
                | Seconds value -> { acc with Seconds = value }
                | Minutes value -> { acc with Minutes = value }
                | Hours value -> { acc with Hours = value })
            { Seconds = 0; Minutes = 0; Hours = 0 }

    let getInterval (value: int) = if value = 0 then "0" else $"0/{value}"

    let buildExpressionString (parameters: CronExpressionParameters) =
        $"{getInterval parameters.Seconds} {getInterval parameters.Minutes} {getInterval parameters.Hours} 1/1 * ? *"

    current
    |> Result.map (fun elements ->
        elements
        |> getParameters
        |> buildExpressionString
        |> CronExpression)