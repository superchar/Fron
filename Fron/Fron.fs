module Fron

open Helpers
open CommonTypes
open Microsoft.FSharp.Core

type ExecutionTime =
    private
    | ParticularTime of int
    | Everytime

type CronExpression = CronExpression of string

type NewExpressionHolder = { Value: int }
type HoursExpressionHolder = { Hours: ExecutionTime }

type MinutesExpressionHolder =
    { Hours: ExecutionTime
      Minutes: ExecutionTime }

type FinalExpressionHolder =
    { Hours: ExecutionTime
      Minutes: ExecutionTime
      Seconds: ExecutionTime }

type HoursExpressionHolderResult = ExpressionErrorResult<HoursExpressionHolder>
type MinutesExpressionHolderResult = ExpressionErrorResult<MinutesExpressionHolder>
type FinalExpressionHolderResult = ExpressionErrorResult<FinalExpressionHolder>
type ExpressionResult = ExpressionErrorResult<CronExpression>

let EverySecondExpressionHolder =
    { Seconds = Everytime
      Minutes = Everytime
      Hours = Everytime }

let ZeroNewExpressionHolder: NewExpressionHolder = { Value = 0 }

let triggerAt (value: int) : NewExpressionHolder = { Value = value }

let triggerAtZero : NewExpressionHolder = ZeroNewExpressionHolder

let andAlso (value: int) (current: ExpressionErrorResult<'a>) : int * ExpressionErrorResult<'a> = value, current

let andAlsoZero (current: ExpressionErrorResult<'a>) : int * ExpressionErrorResult<'a> = andAlso 0 current

let triggerEveryHour: FinalExpressionHolderResult =
    Ok
        { EverySecondExpressionHolder with
            Seconds = ParticularTime 0
            Minutes = ParticularTime 0 }

let hours ({ Value = value }: NewExpressionHolder) : HoursExpressionHolderResult =
    value
    |> tryCreateLimited 24 ParticularTime
    |> Result.map (fun hrs -> { Hours = hrs })

let triggerEveryMinute: FinalExpressionHolderResult =
    Ok { EverySecondExpressionHolder with Seconds = ParticularTime 0 }

let minutes ((value, current): int * HoursExpressionHolderResult) : MinutesExpressionHolderResult =
    current
    |> Result.bind (fun { Hours = hours } ->
        value
        |> tryCreateLessThanSixty ParticularTime
        |> Result.map (fun minutes -> { Hours = hours; Minutes = minutes }))

let triggerEverySecond: FinalExpressionHolderResult =
    Ok EverySecondExpressionHolder

let seconds ((value, current): int * MinutesExpressionHolderResult) : FinalExpressionHolderResult =
    current
    |> Result.bind (fun { Hours = hours; Minutes = minutes } ->
        value
        |> tryCreateLessThanSixty ParticularTime
        |> Result.map (fun sec ->
            { Hours = hours
              Minutes = minutes
              Seconds = sec }))

let toCronExpression (current: FinalExpressionHolderResult) : ExpressionResult =
    let getInterval (executionTime: ExecutionTime) =
        match executionTime with
        | ParticularTime time -> $"{time}"
        | Everytime -> "*"

    let buildExpressionString
        ({ Seconds = seconds
           Minutes = minutes
           Hours = hours }: FinalExpressionHolder)
        : string =
        $"{getInterval seconds} {getInterval minutes} {getInterval hours} ? * *"

    current
    |> Result.map (buildExpressionString >> CronExpression)