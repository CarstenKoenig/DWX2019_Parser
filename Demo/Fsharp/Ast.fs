namespace ParserComb

type Operator =
  | Add | Sub | Mul | Div

type Expression =
  | ValueExpr of double
  | OperatorExpr of Expression * Operator * Expression

module Expression =

  let rec eval =
    function
    | ValueExpr v -> v
    | OperatorExpr (l, Add, r) -> eval l + eval r
    | OperatorExpr (l, Sub, r) -> eval l - eval r
    | OperatorExpr (l, Mul, r) -> eval l * eval r
    | OperatorExpr (l, Div, r) -> eval l / eval r