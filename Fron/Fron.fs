module Fron

open Helpers
open CommonTypes
open Microsoft.FSharp.Core

type ExecutionTime =
    private
    | ParticularTime of int
    | Everytime

type ExecutionDayOfMonth =
    private
    | ParticularDayOfMoth of int
    | EverydayOfMonth

type ExecutionMonth =
    | January
    | February
    | March
    | April
    | May
    | June
    | July
    | August
    | September
    | October
    | November
    | December
    | EveryMonth


type CronExpression = CronExpression of string

type NewExpressionHolder = { Value: int }
type HoursExpressionHolder = { Hours: ExecutionTime }

type MinutesExpressionHolder =
    { Hours: ExecutionTime
      Minutes: ExecutionTime }

type SecondsExpressionHolder =
    { Hours: ExecutionTime
      Minutes: ExecutionTime
      Seconds: ExecutionTime }

type DayOfMonthExpressionHolder =
    { Hours: ExecutionTime
      Minutes: ExecutionTime
      Seconds: ExecutionTime
      DayOfMonth: ExecutionDayOfMonth }

type MonthExpressionHolder =
    { Hours: ExecutionTime
      Minutes: ExecutionTime
      Seconds: ExecutionTime
      DayOfMonth: ExecutionDayOfMonth
      Month: ExecutionMonth }

type HoursExpressionHolderResult = ExpressionErrorResult<HoursExpressionHolder>
type MinutesExpressionHolderResult = ExpressionErrorResult<MinutesExpressionHolder>
type SecondsExpressionHolderResult = ExpressionErrorResult<SecondsExpressionHolder>
type DayOfMonthExpressionHolderResult = ExpressionErrorResult<DayOfMonthExpressionHolder>
type MonthExpressionHolderResult = ExpressionErrorResult<MonthExpressionHolder>
type ExpressionResult = ExpressionErrorResult<CronExpression>

let EverySecondExpressionHolder =
    { Seconds = Everytime
      Minutes = Everytime
      Hours = Everytime
      DayOfMonth = EverydayOfMonth
      Month = EveryMonth }

let ZeroNewExpressionHolder: NewExpressionHolder =
    { Value = 0 }

let triggerAt (value: int) : NewExpressionHolder = { Value = value }

let triggerAtZero: NewExpressionHolder =
    ZeroNewExpressionHolder

let andAlso (value: int) (current: ExpressionErrorResult<'a>) : int * ExpressionErrorResult<'a> = value, current

let andAlsoZero (current: ExpressionErrorResult<'a>) : int * ExpressionErrorResult<'a> = andAlso 0 current

let andAlsoFirst (current: ExpressionErrorResult<'a>) : int * ExpressionErrorResult<'a> = andAlso 1 current

let andAlsoOn
    (month: ExecutionMonth)
    (current: ExpressionErrorResult<'a>)
    : ExecutionMonth * ExpressionErrorResult<'a> =
    month, current

let triggerEveryHour: MonthExpressionHolderResult =

    Ok
        { EverySecondExpressionHolder with
            Seconds = ParticularTime 0
            Minutes = ParticularTime 0 }

let hours ({ Value = value }: NewExpressionHolder) : HoursExpressionHolderResult =
    value
    |> tryCreateLimitedPositive 24 ParticularTime
    |> Result.map (fun hrs -> { Hours = hrs })

let triggerEveryMinute: MonthExpressionHolderResult =
    Ok { EverySecondExpressionHolder with Seconds = ParticularTime 0 }

let minutes ((value, current): int * HoursExpressionHolderResult) : MinutesExpressionHolderResult =
    current
    |> Result.bind (fun { Hours = hours } ->
        value
        |> tryCreateLessThanSixty ParticularTime
        |> Result.map (fun minutes -> { Hours = hours; Minutes = minutes }))

let triggerEverySecond: MonthExpressionHolderResult =
    Ok EverySecondExpressionHolder

let seconds ((value, current): int * MinutesExpressionHolderResult) : SecondsExpressionHolderResult =
    current
    |> Result.bind (fun { Hours = hours; Minutes = minutes } ->
        value
        |> tryCreateLessThanSixty ParticularTime
        |> Result.map (fun sec ->
            { Hours = hours
              Minutes = minutes
              Seconds = sec }))


let triggerEveryDay: MonthExpressionHolderResult =
    Ok
        { EverySecondExpressionHolder with
            Seconds = ParticularTime 0
            Minutes = ParticularTime 0
            Hours = ParticularTime 0 }

let day ((dayValue, current): int * SecondsExpressionHolderResult) : DayOfMonthExpressionHolderResult =
    current
    |> Result.bind
        (fun { Hours = hours
               Minutes = minutes
               Seconds = seconds } ->
            dayValue
            |> tryCreateLimited 1 32 ParticularDayOfMoth
            |> Result.map (fun dayOfMoth ->
                { Hours = hours
                  Minutes = minutes
                  Seconds = seconds
                  DayOfMonth = dayOfMoth }))

let triggerEveryMonth: MonthExpressionHolderResult =
    Ok
        { EverySecondExpressionHolder with
            Seconds = ParticularTime 0
            Minutes = ParticularTime 0
            Hours = ParticularTime 0
            DayOfMonth = ParticularDayOfMoth 1 }

let month ((monthValue, current): ExecutionMonth * DayOfMonthExpressionHolderResult) : MonthExpressionHolderResult =
    current
    |> Result.map
        (fun { Hours = hours
               Minutes = minutes
               Seconds = seconds
               DayOfMonth = dayOfMoth } ->
            { Hours = hours
              Minutes = minutes
              Seconds = seconds
              DayOfMonth = dayOfMoth
              Month = monthValue })

let toCronExpression (current: MonthExpressionHolderResult) : ExpressionResult =
    let getTime (executionTime: ExecutionTime) =
        match executionTime with
        | ParticularTime time -> $"{time}"
        | Everytime -> "*"

    let getDayOfMonth (executionTime: ExecutionDayOfMonth) =
        match executionTime with
        | ParticularDayOfMoth day -> $"{day}"
        | EverydayOfMonth -> "*"

    let getMonth (executionMonth: ExecutionMonth) =
        match executionMonth with
        | January -> "1"
        | February -> "2"
        | March -> "3"
        | April -> "4"
        | May -> "5"
        | June -> "6"
        | July -> "7"
        | August -> "8"
        | September -> "9"
        | October -> "10"
        | November -> "11"
        | December -> "12"
        | EveryMonth -> "*"

    let buildExpressionString
        ({ Seconds = seconds
           Minutes = minutes
           Hours = hours
           DayOfMonth = dayOfMonth
           Month = month }: MonthExpressionHolder)
        : string =
        $"{getTime seconds} {getTime minutes} {getTime hours} {getDayOfMonth dayOfMonth} {getMonth month} ? *"

    current
    |> Result.map (buildExpressionString >> CronExpression)