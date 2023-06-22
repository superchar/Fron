module Fron

open Helpers
open CommonTypes
open Microsoft.FSharp.Core

type ExecutionTime =
    | At of int
    | Every

type ExecutionDayOfMonth =
    | On of int
    | Everyday

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

let trigger (value: ExecutionTime) : ExecutionTime = value

let triggerAtZero : ExecutionTime = trigger (At 0)

let andAlso (value: 'b) (current: ExpressionErrorResult<'a>) : 'b * ExpressionErrorResult<'a> = value, current

let andAlsoZero (current: ExpressionErrorResult<'a>) : ExecutionTime * ExpressionErrorResult<'a> =
    andAlso (At 0) current

let andAlsoFirst (current: ExpressionErrorResult<'a>) : ExecutionDayOfMonth * ExpressionErrorResult<'a> =
    andAlso (On 1) current

let private getPositiveTime (toValue: int) (executionTime: ExecutionTime) =
    match executionTime with
    | At value -> value |> tryCreateLimitedPositive toValue At
    | Every -> Ok executionTime

let hour (executionTime: ExecutionTime) : HoursExpressionHolderResult =
    executionTime
    |> getPositiveTime 24
    |> Result.map (fun hours -> { Hours = hours })

let minute ((value, current): ExecutionTime * HoursExpressionHolderResult) : MinutesExpressionHolderResult =
    current
    |> Result.bind (fun { Hours = hours } ->
        value
        |> getPositiveTime 60
        |> Result.map (fun minutes -> { Hours = hours; Minutes = minutes }))

let second ((value, current): ExecutionTime * MinutesExpressionHolderResult) : SecondsExpressionHolderResult =
    current
    |> Result.bind (fun { Hours = hours; Minutes = minutes } ->
        value
        |> getPositiveTime 60
        |> Result.map (fun sec ->
            { Hours = hours
              Minutes = minutes
              Seconds = sec }))

let day ((dayValue, current): ExecutionDayOfMonth * SecondsExpressionHolderResult) : DayOfMonthExpressionHolderResult =
    let day =
        match dayValue with
        | On value -> value |> tryCreateLimited 1 32 On
        | Everyday -> Ok dayValue

    current
    |> Result.bind
        (fun { Hours = hours
               Minutes = minutes
               Seconds = seconds } ->
            day
            |> Result.map (fun dayOfMoth ->
                { Hours = hours
                  Minutes = minutes
                  Seconds = seconds
                  DayOfMonth = dayOfMoth }))

let ofMonth (monthValue: ExecutionMonth) (current: DayOfMonthExpressionHolderResult) : MonthExpressionHolderResult =
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
        | At time -> $"{time}"
        | Every -> "*"

    let getDayOfMonth (executionTime: ExecutionDayOfMonth) =
        match executionTime with
        | On day -> $"{day}"
        | Everyday -> "*"

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

let everyTime () = Every
let zeroTime () = At 0

let triggerEveryHour =
    everyTime
    >> hour
    >> andAlso (At 0)
    >> minute
    >> andAlso (At 0)
    >> second
    >> andAlso Everyday
    >> day
    >> ofMonth EveryMonth
    >> toCronExpression

let triggerEveryMinute =
    everyTime
    >> hour
    >> andAlso (Every)
    >> minute
    >> andAlso (At 0)
    >> second
    >> andAlso Everyday
    >> day
    >> ofMonth EveryMonth
    >> toCronExpression

let triggerEverySecond =
    everyTime
    >> hour
    >> andAlso (Every)
    >> minute
    >> andAlso (Every)
    >> second
    >> andAlso Everyday
    >> day
    >> ofMonth EveryMonth
    >> toCronExpression

let triggerEveryDay =
    zeroTime
    >> hour
    >> andAlso (At 0)
    >> minute
    >> andAlso (At 0)
    >> second
    >> andAlso Everyday
    >> day
    >> ofMonth EveryMonth
    >> toCronExpression

let triggerEveryMonth =
    zeroTime
    >> hour
    >> andAlso (At 0)
    >> minute
    >> andAlso (At 0)
    >> second
    >> andAlso (On 1)
    >> day
    >> ofMonth EveryMonth
    >> toCronExpression
