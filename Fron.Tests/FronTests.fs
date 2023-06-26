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
    let result = triggerEveryHour ()

    result
    |> should be (ofCase <@ ExpressionResult.Ok @>)

    let (CronExpression value) =
        result |> getLeft

    value |> should be (equal "0 0 * * * ? *")

[<Fact>]
let ``triggerEveryMinute returns the correct Cron expression`` () =
    let result = triggerEveryMinute ()

    result
    |> should be (ofCase <@ ExpressionResult.Ok @>)

    let (CronExpression value) =
        result |> getLeft

    value |> should be (equal "0 * * * * ? *")

[<Fact>]
let ``triggerEverySecond returns the correct Cron expression`` () =
    let result = triggerEverySecond ()

    result
    |> should be (ofCase <@ ExpressionResult.Ok @>)

    let (CronExpression value) =
        result |> getLeft

    value |> should be (equal "* * * * * ? *")

[<Theory>]
[<InlineData(0)>]
[<InlineData(5)>]
[<InlineData(12)>]
[<InlineData(23)>]
let ``when hours are specified the result expression contains hours value`` hoursValue =
    let result =
        trigger (At hoursValue)
        |> hour
        |> andAlsoZero
        |> minute
        |> andAlsoZero
        |> second
        |> andAlsoFirst
        |> day
        |> ofMonth January
        |> toCronExpression

    result
    |> should be (ofCase <@ ExpressionResult.Ok @>)

    let (CronExpression value) =
        result |> getLeft

    value
    |> should be (equal $"0 0 {hoursValue} 1 1 ? *")

[<Theory>]
[<InlineData(-5)>]
[<InlineData(24)>]
[<InlineData(99)>]
let ``when hours are specified with invalid values error is returned`` hoursValue =
    let result =
        trigger (At hoursValue)
        |> hour
        |> andAlsoZero
        |> minute
        |> andAlsoZero
        |> second
        |> andAlsoFirst
        |> day
        |> ofMonth January
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
let ``when minute are specified the result expression contains minute value`` minuteValue =
    let result =
        triggerAtZero
        |> hour
        |> andAlso (At minuteValue)
        |> minute
        |> andAlsoZero
        |> second
        |> andAlsoFirst
        |> day
        |> ofMonth January
        |> toCronExpression

    result
    |> should be (ofCase <@ ExpressionResult.Ok @>)

    let (CronExpression value) =
        result |> getLeft

    value
    |> should be (equal $"0 {minuteValue} 0 1 1 ? *")

[<Theory>]
[<InlineData(-5)>]
[<InlineData(61)>]
[<InlineData(99)>]
let ``when minute are specified with invalid values error is returned`` minuteValue =
    let result =
        triggerAtZero
        |> hour
        |> andAlso (At minuteValue)
        |> minute
        |> andAlsoZero
        |> second
        |> andAlsoFirst
        |> day
        |> ofMonth January
        |> toCronExpression

    result
    |> should be (ofCase <@ ExpressionResult.Error @>)

    result
    |> getErrorMessage
    |> should be (equal $"{minuteValue} is beyond permitted range.")

[<Theory>]
[<InlineData(0)>]
[<InlineData(30)>]
[<InlineData(59)>]
let ``when second are specified the result expression contains second value`` secondValue =
    let result =
        triggerAtZero
        |> hour
        |> andAlsoZero
        |> minute
        |> andAlso (At secondValue)
        |> second
        |> andAlsoFirst
        |> day
        |> ofMonth January
        |> toCronExpression

    result
    |> should be (ofCase <@ ExpressionResult.Ok @>)

    let (CronExpression value) =
        result |> getLeft

    value
    |> should be (equal $"{secondValue} 0 0 1 1 ? *")

[<Theory>]
[<InlineData(-5)>]
[<InlineData(61)>]
[<InlineData(99)>]
let ``when second are specified with invalid values error is returned`` secondValue =
    let result =
        triggerAtZero
        |> hour
        |> andAlsoZero
        |> minute
        |> andAlso (At secondValue)
        |> second
        |> andAlsoFirst
        |> day
        |> ofMonth January
        |> toCronExpression

    result
    |> should be (ofCase <@ ExpressionResult.Error @>)

    result
    |> getErrorMessage
    |> should be (equal $"{secondValue} is beyond permitted range.")

[<Theory>]
[<InlineData(1)>]
[<InlineData(31)>]
[<InlineData(15)>]
let ``when day is specified the result expression contains second value`` daysValue =
    let result =
        triggerAtZero
        |> hour
        |> andAlsoZero
        |> minute
        |> andAlsoZero
        |> second
        |> andAlso (On daysValue)
        |> day
        |> ofMonth January
        |> toCronExpression

    result
    |> should be (ofCase <@ ExpressionResult.Ok @>)

    let (CronExpression value) =
        result |> getLeft

    value
    |> should be (equal $"0 0 0 {daysValue} 1 ? *")

[<Theory>]
[<InlineData(-5)>]
[<InlineData(61)>]
[<InlineData(99)>]
let ``when day is specified with invalid values error is returned`` daysValue =
    let result =
        triggerAtZero
        |> hour
        |> andAlsoZero
        |> minute
        |> andAlsoZero
        |> second
        |> andAlso (On daysValue)
        |> day
        |> ofMonth January
        |> toCronExpression

    result
    |> should be (ofCase <@ ExpressionResult.Error @>)

    result
    |> getErrorMessage
    |> should be (equal $"{daysValue} is beyond permitted range.")

let ``when day is specified with skip value error is returned`` =
    let result =
        triggerAtZero
        |> hour
        |> andAlsoZero
        |> minute
        |> andAlsoZero
        |> second
        |> andAlso SkipDayOfMonth
        |> day
        |> ofMonth January
        |> toCronExpression

    result
    |> should be (ofCase <@ ExpressionResult.Error @>)

    result
    |> getErrorMessage
    |> should be (equal "Cannot skip both day and day of week")

[<Fact>]
let ``when day of week is specified the result expression contains second value`` =
    let result =
        triggerAtZero
        |> hour
        |> andAlsoZero
        |> minute
        |> andAlsoZero
        |> second
        |> andAlso Tuesday
        |> dayOfWeek
        |> ofMonth January
        |> toCronExpression

    result
    |> should be (ofCase <@ ExpressionResult.Ok @>)

    let (CronExpression value) =
        result |> getLeft

    value |> should be (equal $"0 0 0 ? 1 TUE *")

[<Theory>]
[<InlineData(-5)>]
[<InlineData(61)>]
[<InlineData(99)>]
let ``when day of week is specified with skip value error is returned`` =
    let result =
        triggerAtZero
        |> hour
        |> andAlsoZero
        |> minute
        |> andAlsoZero
        |> second
        |> andAlso SkipDayOfWeek
        |> dayOfWeek
        |> ofMonth January
        |> toCronExpression

    result
    |> should be (ofCase <@ ExpressionResult.Error @>)

    result
    |> getErrorMessage
    |> should be (equal "Cannot skip both day and day of week")

[<Fact>]
let ``when all the values are specified the result expression contains all the values`` () =
    let result =
        trigger (At 13)
        |> hour
        |> andAlso (At 5)
        |> minute
        |> andAlso (At 11)
        |> second
        |> andAlso (On 6)
        |> day
        |> ofMonth February
        |> toCronExpression

    result
    |> should be (ofCase <@ ExpressionResult.Ok @>)

    let (CronExpression value) =
        result |> getLeft

    value |> should be (equal "11 5 13 6 2 ? *")

[<Fact>]
let ``when values are all invalid returns first error`` () =
    let hoursValue = 25
    let minuteValue = 61
    let secondValue = 61
    let daysValue = 32

    let result =
        trigger (At hoursValue)
        |> hour
        |> andAlso (At minuteValue)
        |> minute
        |> andAlso (At secondValue)
        |> second
        |> andAlso (On daysValue)
        |> day
        |> ofMonth January
        |> toCronExpression

    result
    |> should be (ofCase <@ ExpressionResult.Error @>)

    result
    |> getErrorMessage
    |> should be (equal $"{hoursValue} is beyond permitted range.")

[<Fact>]
let ``finishOnHoursRestAreEvery should set the rest to every`` () =
    let result =
        trigger (At 13)
        |> hour
        |> finishOnHoursRestAreEvery
        |> toCronExpression

    result
    |> should be (ofCase <@ ExpressionResult.Ok @>)

    let (CronExpression value) =
        result |> getLeft

    value |> should be (equal "* * 13 * * ? *")

[<Fact>]
let ``finishOnMinutesRestAreEvery should set the rest to every`` () =
    let result =
        trigger (At 13)
        |> hour
        |> andAlso (At 45)
        |> minute
        |> finishOnMinutesRestAreEvery
        |> toCronExpression

    result
    |> should be (ofCase <@ ExpressionResult.Ok @>)

    let (CronExpression value) =
        result |> getLeft

    value |> should be (equal "* 45 13 * * ? *")

[<Fact>]
let ``finishOnSecondsRestAreEvery should set the rest to every`` () =
    let result =
        trigger (At 13)
        |> hour
        |> andAlso (At 45)
        |> minute
        |> andAlso (At 30)
        |> second
        |> finishOnSecondsRestAreEvery
        |> toCronExpression

    result
    |> should be (ofCase <@ ExpressionResult.Ok @>)

    let (CronExpression value) =
        result |> getLeft

    value |> should be (equal "30 45 13 * * ? *")

[<Fact>]
let ``finishOnDaysRestAreEvery should set the rest to every`` () =
    let result =
        trigger (At 13)
        |> hour
        |> andAlso (At 45)
        |> minute
        |> andAlso (At 30)
        |> second
        |> andAlso (On 5)
        |> day
        |> finishOnDaysRestAreEvery
        |> toCronExpression

    result
    |> should be (ofCase <@ ExpressionResult.Ok @>)

    let (CronExpression value) =
        result |> getLeft

    value |> should be (equal "30 45 13 5 * ? *")

[<Fact>]
let ``finishOnDaysOfWeekRestAreEvery should set the rest to every`` () =
    let result =
        trigger (At 13)
        |> hour
        |> andAlso (At 45)
        |> minute
        |> andAlso (At 30)
        |> second
        |> andAlso Monday
        |> dayOfWeek
        |> finishOnDaysOfWeekRestAreEvery
        |> toCronExpression

    result
    |> should be (ofCase <@ ExpressionResult.Ok @>)

    let (CronExpression value) =
        result |> getLeft

    value |> should be (equal "30 45 13 ? * MON *")