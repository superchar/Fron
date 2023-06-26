module Fron.Types

type ExpressionError = ExpressionError of string

type ExpressionErrorResult<'a> = Result<'a, ExpressionError>

type ExecutionTime =
    | At of int
    | Every

type ExecutionYear =
    | In of int
    | Each

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

type ExecutionDay =
    | DayOfMonth of ExecutionDayOfMonth
    | DayOfWeek of ExecutionDayOfWeek

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
      Day: ExecutionDay }

type MonthExpressionHolder =
    { Hours: ExecutionTime
      Minutes: ExecutionTime
      Seconds: ExecutionTime
      Day: ExecutionDay
      Month: ExecutionMonth }

type YearExpressionHolder =
    { Hours: ExecutionTime
      Minutes: ExecutionTime
      Seconds: ExecutionTime
      Day: ExecutionDay
      Month: ExecutionMonth
      Year: ExecutionYear }

type ExpressionHolder =
    | Hours of HoursExpressionHolder
    | Minutes of MinutesExpressionHolder
    | Seconds of SecondsExpressionHolder
    | Day of DayExpressionHolder
    | Month of MonthExpressionHolder
    | Year of YearExpressionHolder

type HoursExpressionHolderResult = ExpressionErrorResult<HoursExpressionHolder>
type MinutesExpressionHolderResult = ExpressionErrorResult<MinutesExpressionHolder>
type SecondsExpressionHolderResult = ExpressionErrorResult<SecondsExpressionHolder>
type DayExpressionHolderResult = ExpressionErrorResult<DayExpressionHolder>
type MonthExpressionHolderResult = ExpressionErrorResult<MonthExpressionHolder>
type ExpressionResult = ExpressionErrorResult<CronExpression>
type ExpressionHolderResult = ExpressionErrorResult<ExpressionHolder>
