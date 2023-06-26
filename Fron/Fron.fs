module Fron

open Helpers
open CommonTypes
open Microsoft.FSharp.Core

type ExecutionTime =
    | At of int
    | Every

type ExecutionDayOfMonth =
    | On of int
    | EveryDayOfMonth
    | SkipDayOfMonth

type ExecutionDayOfWeek =
    | Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday
    | SkipDayOfWeek
    | EveryDayOfWeek

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

type DayExpressionHolder =
    { Hours: ExecutionTime
      Minutes: ExecutionTime
      Seconds: ExecutionTime
      DayOfWeek: ExecutionDayOfWeek
      DayOfMonth: ExecutionDayOfMonth }

type MonthExpressionHolder =
    { Hours: ExecutionTime
      Minutes: ExecutionTime
      Seconds: ExecutionTime
      DayOfWeek: ExecutionDayOfWeek
      DayOfMonth: ExecutionDayOfMonth
      Month: ExecutionMonth }

type ExpressionHolder =
    | Hours of HoursExpressionHolder
    | Minutes of MinutesExpressionHolder
    | Seconds of SecondsExpressionHolder
    | DayOfMoth of DayExpressionHolder
    | DayOfWeek of DayExpressionHolder
    | Month of MonthExpressionHolder

type HoursExpressionHolderResult = ExpressionErrorResult<HoursExpressionHolder>
type MinutesExpressionHolderResult = ExpressionErrorResult<MinutesExpressionHolder>
type SecondsExpressionHolderResult = ExpressionErrorResult<SecondsExpressionHolder>
type DayExpressionHolderResult = ExpressionErrorResult<DayExpressionHolder>
type MonthExpressionHolderResult = ExpressionErrorResult<MonthExpressionHolder>
type ExpressionResult = ExpressionErrorResult<CronExpression>
type ExpressionHolderResult = ExpressionErrorResult<ExpressionHolder>

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
                  DayOfWeek = SkipDayOfWeek
                  DayOfMonth = dayOfMoth }))

let finishOnDaysRestAreEvery (current: DayExpressionHolderResult) = current |> Result.map DayOfMoth

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
                  DayOfWeek = currentDayValue
                  DayOfMonth = SkipDayOfMonth }))

let finishOnDaysOfWeekRestAreEvery (current: DayExpressionHolderResult) = current |> Result.map DayOfWeek

let ofMonth (monthValue: ExecutionMonth) (current: DayExpressionHolderResult) : ExpressionHolderResult =
    current
    |> Result.map
        (fun { Hours = hours
               Minutes = minutes
               Seconds = seconds
               DayOfWeek = dayOfWeek
               DayOfMonth = dayOfMoth } ->
            Month
                { Hours = hours
                  Minutes = minutes
                  Seconds = seconds
                  DayOfWeek = dayOfWeek
                  DayOfMonth = dayOfMoth
                  Month = monthValue })

let EverySecondExpressionHolder =
    { Hours = Every
      Minutes = Every
      Seconds = Every
      DayOfMonth = EveryDayOfMonth
      DayOfWeek = SkipDayOfWeek
      Month = EveryMonth }

let toCronExpression (current: ExpressionHolderResult) : ExpressionResult =
    let getFinalExpressionHolder (holder: ExpressionHolder) : MonthExpressionHolder =
        match holder with
        | Hours { Hours = hours } -> { EverySecondExpressionHolder with Hours = hours }
        | Minutes { Hours = hours; Minutes = minutes } ->
            { EverySecondExpressionHolder with
                Hours = hours
                Minutes = minutes }
        | Seconds { Hours = hours
                    Minutes = minutes
                    Seconds = seconds } ->
            { EverySecondExpressionHolder with
                Hours = hours
                Minutes = minutes
                Seconds = seconds }
        | DayOfMoth { Hours = hours
                      Minutes = minutes
                      Seconds = seconds
                      DayOfMonth = dayOfMonth } ->
            { EverySecondExpressionHolder with
                Hours = hours
                Minutes = minutes
                Seconds = seconds
                DayOfMonth = dayOfMonth
                DayOfWeek = SkipDayOfWeek }
        | DayOfWeek { Hours = hours
                      Minutes = minutes
                      Seconds = seconds
                      DayOfWeek = dayOfWeek } ->
            { EverySecondExpressionHolder with
                Hours = hours
                Minutes = minutes
                Seconds = seconds
                DayOfMonth = SkipDayOfMonth
                DayOfWeek = dayOfWeek }
        | Month value -> value

    let getTime (executionTime: ExecutionTime) =
        match executionTime with
        | At time -> $"{time}"
        | Every -> "*"

    let getDayOfMonth (executionTime: ExecutionDayOfMonth) =
        match executionTime with
        | On day -> $"{day}"
        | EveryDayOfMonth -> "*"
        | SkipDayOfMonth -> "?"

    let getDayOfWeek (executionTime: ExecutionDayOfWeek) =
        match executionTime with
        | Monday -> "MON"
        | Tuesday -> "TUE"
        | Wednesday -> "WED"
        | Thursday -> "THU"
        | Friday -> "FRI"
        | Saturday -> "SAT"
        | Sunday -> "SUN"
        | EveryDayOfWeek -> "SUN-SAT"
        | SkipDayOfWeek -> "?"

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
           DayOfWeek = dayOfWeek
           DayOfMonth = dayOfMonth
           Month = month }: MonthExpressionHolder)
        : string =
        $"{getTime seconds} {getTime minutes} {getTime hours} {getDayOfMonth dayOfMonth} {getMonth month} {getDayOfWeek dayOfWeek} *"

    current
    |> Result.map (
        getFinalExpressionHolder
        >> buildExpressionString
        >> CronExpression
    )

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
    >> toCronExpression

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
    >> toCronExpression

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
    >> toCronExpression

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