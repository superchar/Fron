module FronTests

open CommonTypes
open Fron
open FsUnit
open FsUnit.CustomMatchers
open Xunit

let private getLeft (expression: ExpressionResult) : CronExpression =
    expression
    |> Result.defaultValue (CronExpression "")

let private getErrorMessage (expression: ExpressionResult) : string =
    match expression with
    | Error (ExpressionError msg) -> msg
    | _ -> ""

[<Fact>]
let ``triggerEveryHour returns the correct Cron expression`` () =
    let result =
        triggerEveryHour |> toCronExpression

    result
    |> should be (ofCase <@ ExpressionResult.Ok @>)

    let (CronExpression value) =
        result |> getLeft

    value |> should be (equal "0 0 * ? * *")

[<Fact>]
let ``triggerEveryMinute returns the correct Cron expression`` () =
    let result =
        triggerEveryMinute |> toCronExpression

    result
    |> should be (ofCase <@ ExpressionResult.Ok @>)

    let (CronExpression value) =
        result |> getLeft

    value |> should be (equal "0 * * ? * *")

[<Fact>]
let ``triggerEverySecond returns the correct Cron expression`` () =
    let result =
        triggerEveryMinute |> toCronExpression

    result
    |> should be (ofCase <@ ExpressionResult.Ok @>)

    let (CronExpression value) =
        result |> getLeft

    value |> should be (equal "0 * * ? * *")

[<Theory>]
[<InlineData(0)>]
[<InlineData(5)>]
[<InlineData(12)>]
[<InlineData(23)>]
let ``when hours are specified the result expression contains hours value`` hoursValue =
    let result =
        triggerAt hoursValue
        |> hours
        |> andAlsoZero
        |> minutes
        |> andAlsoZero
        |> seconds
        |> toCronExpression

    result
    |> should be (ofCase <@ ExpressionResult.Ok @>)

    let (CronExpression value) =
        result |> getLeft

    value
    |> should be (equal $"0 0 {hoursValue} ? * *")

[<Theory>]
[<InlineData(-5)>]
[<InlineData(24)>]
[<InlineData(99)>]
let ``when hours are specified with invalid values error is returned`` hoursValue =
    let result =
        triggerAt hoursValue
        |> hours
        |> andAlsoZero
        |> minutes
        |> andAlsoZero
        |> seconds
        |> toCronExpression

    result
    |> should be (ofCase <@ ExpressionResult.Error @>)

    result
    |> getErrorMessage
    |> should be (equal $"{hoursValue} is beyond permitted range.")

[<Theory>]
[<InlineData(0)>]
[<InlineData(30)>]
[<InlineData(59)>]
let ``when minutes are specified the result expression contains minutes value`` minutesValue =
    let result =
        triggerAtZero
        |> hours
        |> andAlso minutesValue
        |> minutes
        |> andAlsoZero
        |> seconds
        |> toCronExpression

    result
    |> should be (ofCase <@ ExpressionResult.Ok @>)

    let (CronExpression value) =
        result |> getLeft

    value
    |> should be (equal $"0 {minutesValue} 0 ? * *")

[<Theory>]
[<InlineData(-5)>]
[<InlineData(61)>]
[<InlineData(99)>]
let ``when minutes are specified with invalid values error is returned`` minutesValue =
    let result =
        triggerAtZero
        |> hours
        |> andAlso minutesValue
        |> minutes
        |> andAlsoZero
        |> seconds
        |> toCronExpression

    result
    |> should be (ofCase <@ ExpressionResult.Error @>)

    result
    |> getErrorMessage
    |> should be (equal $"{minutesValue} is beyond permitted range.")

[<Theory>]
[<InlineData(0)>]
[<InlineData(30)>]
[<InlineData(59)>]
let ``when seconds are specified the result expression contains seconds value`` secondValue =
    let result =
        triggerAtZero
        |> hours
        |> andAlsoZero
        |> minutes
        |> andAlso secondValue
        |> seconds
        |> toCronExpression

    result
    |> should be (ofCase <@ ExpressionResult.Ok @>)

    let (CronExpression value) =
        result |> getLeft

    value
    |> should be (equal $"{secondValue} 0 0 ? * *")

[<Theory>]
[<InlineData(-5)>]
[<InlineData(61)>]
[<InlineData(99)>]
let ``when seconds are specified with invalid values error is returned`` secondsValue =
    let result =
        triggerAtZero
        |> hours
        |> andAlso secondsValue
        |> minutes
        |> andAlso secondsValue
        |> seconds
        |> toCronExpression

    result
    |> should be (ofCase <@ ExpressionResult.Error @>)

    result
    |> getErrorMessage
    |> should be (equal $"{secondsValue} is beyond permitted range.")

[<Fact>]
let ``when hours, minutes and seconds are specified the result expression contains all the values`` () =
    let result =
        triggerAt 13
        |> hours
        |> andAlso 5
        |> minutes
        |> andAlso 11
        |> seconds
        |> toCronExpression

    result
    |> should be (ofCase <@ ExpressionResult.Ok @>)

    let (CronExpression value) =
        result |> getLeft

    value |> should be (equal "11 5 13 ? * *")