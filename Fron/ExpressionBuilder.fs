module Fron.ExpressionBuilder

open Fron.Types

let private everyTime = "*"
let private timeIsUndefined = "?"

let private EverySecondExpressionHolder =
    { Hours = Every
      Minutes = Every
      Seconds = Every
      Day = DayOfMonth <| EveryDayOfMonth
      Month = EveryMonth
      Year = Each }

let private getFinalExpressionHolder (holder: ExpressionHolder) : YearExpressionHolder =
    match holder with
    | Hours value -> { EverySecondExpressionHolder with Hours = value.Hours }
    | Minutes value ->
        { EverySecondExpressionHolder with
            Hours = value.Hours
            Minutes = value.Minutes }
    | Seconds value ->
        { EverySecondExpressionHolder with
            Hours = value.Hours
            Minutes = value.Minutes
            Seconds = value.Seconds }
    | Day value ->
        { EverySecondExpressionHolder with
            Hours = value.Hours
            Minutes = value.Minutes
            Seconds = value.Seconds
            Day = value.Day }
    | Month value ->
        { EverySecondExpressionHolder with
            Hours = value.Hours
            Minutes = value.Minutes
            Seconds = value.Seconds
            Day = value.Day
            Month = value.Month }
    | Year year -> year

let private getTime (executionTime: ExecutionTime) =
    match executionTime with
    | At time -> $"{time}"
    | Every -> everyTime

let private getDayOfMonth (executionDay: ExecutionDay) =
    match executionDay with
    | DayOfMonth dayOfMonth ->
        match dayOfMonth with
        | On day -> $"{day}"
        | EveryDayOfMonth -> everyTime
        | SkipDayOfMonth -> timeIsUndefined
    | _ -> timeIsUndefined

let private getDayOfWeek (executionDay: ExecutionDay) =
    match executionDay with
    | DayOfWeek dayOfWeek ->
        match dayOfWeek with
        | Monday -> "MON"
        | Tuesday -> "TUE"
        | Wednesday -> "WED"
        | Thursday -> "THU"
        | Friday -> "FRI"
        | Saturday -> "SAT"
        | Sunday -> "SUN"
        | EveryDayOfWeek -> "SUN-SAT"
        | SkipDayOfWeek -> timeIsUndefined
    | _ -> timeIsUndefined

let private getMonth (executionMonth: ExecutionMonth) =
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
    | EveryMonth -> everyTime

let private getYear (executionTime: ExecutionYear) =
    match executionTime with
    | In year -> $"{year}"
    | Each -> everyTime

let private buildExpressionString
    ({ Seconds = seconds
       Minutes = minutes
       Hours = hours
       Day = day
       Month = month
       Year = year }: YearExpressionHolder)
    : string =
    $"{getTime seconds} {getTime minutes} {getTime hours} {getDayOfMonth day} {getMonth month} {getDayOfWeek day} {getYear year}"

let build (current: ExpressionHolderResult) : ExpressionResult =
    current
    |> Result.map (
        getFinalExpressionHolder
        >> buildExpressionString
        >> CronExpression
    )
