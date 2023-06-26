module Fron.Api

open Fron.Helpers
open Fron.Types
open Fron.ExpressionBuilder
open Microsoft.FSharp.Core

let trigger (value: ExecutionTime) : ExecutionTime = value

let triggerAtZero: ExecutionTime =
    trigger (At 0)

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

let finishOnHoursRestAreEvery (current: HoursExpressionHolderResult) = current |> Result.map Hours

let minute ((value, current): ExecutionTime * HoursExpressionHolderResult) : MinutesExpressionHolderResult =
    current
    |> Result.bind (fun { Hours = hours } ->
        value
        |> getPositiveTime 60
        |> Result.map (fun minutes -> { Hours = hours; Minutes = minutes }))

let finishOnMinutesRestAreEvery (current: MinutesExpressionHolderResult) = current |> Result.map Minutes

let second ((value, current): ExecutionTime * MinutesExpressionHolderResult) : SecondsExpressionHolderResult =
    current
    |> Result.bind (fun { Hours = hours; Minutes = minutes } ->
        value
        |> getPositiveTime 60
        |> Result.map (fun sec ->
            { Hours = hours
              Minutes = minutes
              Seconds = sec }))

let finishOnSecondsRestAreEvery (current: SecondsExpressionHolderResult) = current |> Result.map Seconds

let private cannotSkipDayAndDayOfWeekError =
    ExpressionError "Cannot skip both day and day of week"

let day ((dayValue, current): ExecutionDayOfMonth * SecondsExpressionHolderResult) : DayExpressionHolderResult =
    let day =
        match dayValue with
        | On value -> value |> tryCreateLimited 1 32 On
        | EveryDayOfMonth -> Ok dayValue
        | SkipDayOfMonth -> Error cannotSkipDayAndDayOfWeekError

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
                  Day = DayOfMonth dayOfMoth }))

let finishOnDaysRestAreEvery (current: DayExpressionHolderResult) = current |> Result.map Day

let dayOfWeek ((dayValue, current): ExecutionDayOfWeek * SecondsExpressionHolderResult) : DayExpressionHolderResult =

    current
    |> Result.bind
        (fun { Hours = hours
               Minutes = minutes
               Seconds = seconds } ->
            let dayValueResult =
                match dayValue with
                | SkipDayOfWeek -> Error cannotSkipDayAndDayOfWeekError
                | _ -> Ok dayValue

            dayValueResult
            |> Result.map (fun currentDayValue ->
                { Hours = hours
                  Minutes = minutes
                  Seconds = seconds
                  Day = DayOfWeek currentDayValue }))

let finishOnDaysOfWeekRestAreEvery (current: DayExpressionHolderResult) = current |> Result.map Day

let ofMonth (monthValue: ExecutionMonth) (current: DayExpressionHolderResult) : MonthExpressionHolderResult =
    current
    |> Result.map
        (fun { Hours = hours
               Minutes = minutes
               Seconds = seconds
               Day = day } ->
            { Hours = hours
              Minutes = minutes
              Seconds = seconds
              Day = day
              Month = monthValue })

let finishOnMonthsRestAreEvery (current: MonthExpressionHolderResult) = current |> Result.map Month

let year ((year, current): ExecutionYear * MonthExpressionHolderResult) : ExpressionHolderResult =
    let yearResult =
        match year with
        | In value -> tryCreateLimited 2023 3000 In value
        | Each -> Ok year

    current
    |> Result.bind
        (fun { Hours = hours
               Minutes = minutes
               Seconds = seconds
               Day = day
               Month = month } ->
            yearResult
            |> Result.map (fun year ->
                Year
                    { Hours = hours
                      Minutes = minutes
                      Seconds = seconds
                      Day = day
                      Month = month
                      Year = year }))

let everyTime () = Every
let zeroTime () = At 0

let triggerEveryHour =
    everyTime
    >> hour
    >> andAlso (At 0)
    >> minute
    >> andAlso (At 0)
    >> second
    >> andAlso EveryDayOfMonth
    >> day
    >> ofMonth EveryMonth
    >> andAlso Each
    >> year
    >> build

let triggerEveryMinute =
    everyTime
    >> hour
    >> andAlso Every
    >> minute
    >> andAlso (At 0)
    >> second
    >> andAlso EveryDayOfMonth
    >> day
    >> ofMonth EveryMonth
    >> andAlso Each
    >> year
    >> build

let triggerEverySecond =
    everyTime
    >> hour
    >> andAlso Every
    >> minute
    >> andAlso Every
    >> second
    >> andAlso EveryDayOfMonth
    >> day
    >> ofMonth EveryMonth
    >> andAlso Each
    >> year
    >> build

let triggerEveryDay =
    zeroTime
    >> hour
    >> andAlso (At 0)
    >> minute
    >> andAlso (At 0)
    >> second
    >> andAlso EveryDayOfMonth
    >> day
    >> ofMonth EveryMonth
    >> andAlso Each
    >> year
    >> build

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
    >> andAlso Each
    >> year
    >> build