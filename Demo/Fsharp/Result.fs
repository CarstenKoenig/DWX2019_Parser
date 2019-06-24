namespace ParserComb

type Result<'a> =
    | Success of output:'a * remainingInput:string
    | Failure

module Result =

    let map (f : 'a -> 'b) (r : Result<'a>) : Result<'b> =
        match r with
        | Success (value, remaining) -> Success (f value, remaining)
        | Failure -> Failure

    let tryMap (tryF : 'a -> bool * 'b) (r : Result<'a>) : Result<'b> =
        match r with
        | Success (value, remaining) -> 
            match tryF value with
            | (true, outValue) -> Success (outValue, remaining)
            | _ -> Failure
        | Failure -> Failure