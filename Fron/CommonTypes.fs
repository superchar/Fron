module CommonTypes

type ExpressionError = ExpressionError of string

type ExpressionErrorResult<'a> = Result<'a, ExpressionError>