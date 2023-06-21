module Fron

open Fron.Helpers
open Fron.CommonTypes
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

type NewCronExpression = { Value: int }
type HoursCronExpression = { Hours: Hours }
type MinutesCronExpression = { Hours: Hours; Minutes: Minutes }

type FinalCronExpression =
    { Hours: Hours
      Minutes: Minutes
      Seconds: Seconds }

type BuildExpressionResult<'a> = Result<'a, ExpressionError>
type HoursExpressionResult = BuildExpressionResult<HoursCronExpression>
type MinutesExpressionResult = BuildExpressionResult<MinutesCronExpression>
type FinalExpressionResult = BuildExpressionResult<FinalCronExpression>
type ExpressionResult = Result<CronExpression, ExpressionError>

let EverySecondCronExpression =
    { Seconds = EverySecond
      Minutes = EveryMinute
      Hours = EveryHour }

let triggerAt (value: int) : NewCronExpression = { Value = value }

let andAlso (value: int) (current: BuildExpressionResult<'a>) : int * BuildExpressionResult<'a> = value, current

let andAlsoZero (current: BuildExpressionResult<'a>) : int * BuildExpressionResult<'a> = 0, current

let triggerEveryHour: FinalExpressionResult =
    Ok
        { EverySecondCronExpression with
            Seconds = Seconds 0
            Minutes = Minutes 0 }

let hours ({ Value = value }: NewCronExpression) : HoursExpressionResult =
    value
    |> tryCreateLimited 0 24 Hours
    |> Result.map (fun hrs -> { Hours = hrs })

let triggerEveryMinute: FinalExpressionResult =
    Ok { EverySecondCronExpression with Seconds = Seconds 0 }

let minutes ((value, current): int * HoursExpressionResult) : MinutesExpressionResult =
    current
    |> Result.bind (fun { Hours = hours } ->
        value
        |> tryCreateInZeroSixtyRange Minutes
        |> Result.map (fun minutes -> { Hours = hours; Minutes = minutes }))

let triggerEverySecond: FinalExpressionResult =
    Ok EverySecondCronExpression

let seconds ((value, current): int * MinutesExpressionResult) : FinalExpressionResult =
    current
    |> Result.bind (fun { Hours = hours; Minutes = minutes } ->
        value
        |> tryCreateInZeroSixtyRange Seconds
        |> Result.map (fun sec ->
            { Hours = hours
              Minutes = minutes
              Seconds = sec }))

let generate (current: Result<FinalCronExpression, ExpressionError>) : ExpressionResult =
    let buildExpressionString
        ({ Seconds = seconds
           Minutes = minutes
           Hours = hours }: FinalCronExpression)
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