module Fron

open Helpers
open CommonTypes
open Microsoft.FSharp.Core

type Hours =
    private
    | Hours of int
    | EveryHour

type Minutes =
    private
    | Minutes of int
    | EveryMinute

type Seconds =
    private
    | Seconds of int
    | EverySecond

type CronExpression = CronExpression of string

type NewExpressionHolder = { Value: int }
type HoursExpressionHolder = { Hours: Hours }
type MinutesExpressionHolder = { Hours: Hours; Minutes: Minutes }

type FinalExpressionHolder =
    { Hours: Hours
      Minutes: Minutes
      Seconds: Seconds }

type ExpressionErrorResult<'a> = Result<'a, ExpressionError>
type HoursExpressionHolderResult = ExpressionErrorResult<HoursExpressionHolder>
type MinutesExpressionHolderResult = ExpressionErrorResult<MinutesExpressionHolder>
type FinalExpressionHolderResult = ExpressionErrorResult<FinalExpressionHolder>
type ExpressionResult = ExpressionErrorResult<CronExpression>

let EverySecondExpressionHolder =
    { Seconds = EverySecond
      Minutes = EveryMinute
      Hours = EveryHour }

let triggerAt (value: int) : NewExpressionHolder = { Value = value }

let andAlso (value: int) (current: ExpressionErrorResult<'a>) : int * ExpressionErrorResult<'a> = value, current

let andAlsoZero (_: ExpressionErrorResult<'a>) : ExpressionErrorResult<'a> -> int * ExpressionErrorResult<'a> =
    andAlso 0

let triggerEveryHour: FinalExpressionHolderResult =
    Ok
        { EverySecondExpressionHolder with
            Seconds = Seconds 0
            Minutes = Minutes 0 }

let hours ({ Value = value }: NewExpressionHolder) : HoursExpressionHolderResult =
    value
    |> tryCreateLimited 0 24 Hours
    |> Result.map (fun hrs -> { Hours = hrs })

let triggerEveryMinute: FinalExpressionHolderResult =
    Ok { EverySecondExpressionHolder with Seconds = Seconds 0 }

let minutes ((value, current): int * HoursExpressionHolderResult) : MinutesExpressionHolderResult =
    current
    |> Result.bind (fun { Hours = hours } ->
        value
        |> tryCreateInZeroSixtyRange Minutes
        |> Result.map (fun minutes -> { Hours = hours; Minutes = minutes }))

let triggerEverySecond: FinalExpressionHolderResult =
    Ok EverySecondExpressionHolder

let seconds ((value, current): int * MinutesExpressionHolderResult) : FinalExpressionHolderResult =
    current
    |> Result.bind (fun { Hours = hours; Minutes = minutes } ->
        value
        |> tryCreateInZeroSixtyRange Seconds
        |> Result.map (fun sec ->
            { Hours = hours
              Minutes = minutes
              Seconds = sec }))

let toCronExpression (current: FinalExpressionHolderResult) : ExpressionResult =
    let buildExpressionString
        ({ Seconds = seconds
           Minutes = minutes
           Hours = hours }: FinalExpressionHolder)
        : string =
        let hoursInterval =
            match hours with
            | Hours value -> $"{value}"
            | EveryHour -> "*"

        let minutesInterval =
            match minutes with
            | Minutes value -> $"{value}"
            | EveryMinute -> "*"

        let secondsInterval =
            match seconds with
            | Seconds value -> $"{value}"
            | EverySecond -> "*"

        $"{secondsInterval} {minutesInterval} {hoursInterval} ? * *"

    current
    |> Result.map (buildExpressionString >> CronExpression)