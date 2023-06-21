module FronTests

open Fron
open FsUnit
open FsUnit.CustomMatchers
open Xunit

let private getLeft (expression: ExpressionResult): CronExpression =
    expression
    |> Result.defaultValue (CronExpression "")

[<Fact>]
let ``triggerEveryHour returns the correct Cron expression`` () =
    let result = triggerEveryHour |> toCronExpression

    result
    |> should be (ofCase <@ ExpressionResult.Ok @>)
     
    let (CronExpression value) = result |> getLeft
     
    value |> should be (equal "0 0 * ? * *")
    
[<Fact>]
let ``triggerEveryMinute returns the correct Cron expression`` () =
    let result = triggerEveryMinute |> toCronExpression

    result
    |> should be (ofCase <@ ExpressionResult.Ok @>)
     
    let (CronExpression value) = result |> getLeft
     
    value |> should be (equal "0 * * ? * *")
    
[<Fact>]
let ``triggerEverySecond returns the correct Cron expression`` () =
    let result = triggerEveryMinute |> toCronExpression

    result
    |> should be (ofCase <@ ExpressionResult.Ok @>)
     
    let (CronExpression value) = result |> getLeft
     
    value |> should be (equal "0 * * ? * *")