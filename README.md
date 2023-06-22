# Fron
F# cron expression builder
Example _"At 13:05:11pm, on the 6th day, in February"_

```F#
trigger (At 13) |> hour
|> andAlso (At 5) |> minute
|> andAlso (At 11) |> second
|> andAlso (On 6) |> day |> ofMonth February 
|> toCronExpression
```
