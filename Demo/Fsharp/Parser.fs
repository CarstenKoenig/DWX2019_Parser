namespace ParserComb
open System.Diagnostics

type Parser<'a> = string -> Result<'a>


module Parser =

    [<DebuggerStepThrough>]
    let run (p : Parser<'a>) (input : string) =
        p input

    let fail () : Parser<'a> =
        fun _ -> Failure

    let succeed (withValue : 'a) : Parser<'a> =
        fun input -> Success (withValue, input)

    let map (f : 'a -> 'b) (p : Parser<'a>) : Parser<'b> =
        fun input -> run p input |> Result.map f

    let tryMap tryF (p : Parser<'a>) : Parser<'b> =
        fun input -> run p input |> Result.tryMap tryF  

    let apMap (pf : Parser<'a -> 'b>) (pa : Parser<'a>) : Parser<'b> =
        fun input ->
            match run pf input with
            | Failure -> Failure
            | Success (f, remainingF) ->
                match run pa remainingF with
                | Failure -> Failure
                | Success (a, remainingA) ->
                    Success (f a, remainingA)

    [<DebuggerStepThrough>]
    let (<*>) pf a = apMap pf a

    [<DebuggerStepThrough>]
    let build = succeed

    [<DebuggerStepThrough>]
    let withParser a pf = pf <*> a

    [<DebuggerStepThrough>]
    let andThen (decide : 'a -> Parser<'b>) (p : Parser<'a>) : Parser<'b> =
        fun input ->
            match run p input with
            | Success (valueA, remaining) -> 
                run (decide valueA) remaining
            | Failure -> Failure

    [<DebuggerStepThrough>]
    let (>>=) p f = p |> andThen f

    type ParserBuilder() =
        [<DebuggerStepThrough>]
        member __.Bind(p,f) = p |> andThen f
        [<DebuggerStepThrough>]
        member __.Return(x) = succeed x
        [<DebuggerStepThrough>]
        member __.ReturnFrom x = x

    let parser = ParserBuilder ()

    let character (isValid : char -> bool) : Parser<char> =
        fun input ->
            if input.Length = 0 || not (isValid input.[0])
            then Failure
            else Success (input.[0], input.[1..])

    let exactChar (expected : char) : Parser<unit> =
        character (fun c -> c = expected)
        |> map ignore

    let alternative (either : Parser<'a>) (orElse : Parser<'a>) =
        fun input ->
            match run either input with
            | Success _ as res -> res
            | Failure          -> run orElse input

    let choice (parsers : Parser<'a> seq) =
        fun input ->
            let rec walkList =
                function
                | [] -> Result<'a>.Failure
                | p::ps ->
                    match run p input with
                    | Success _ as res -> res
                    | Failure ->  walkList ps
            walkList (Seq.toList parsers)

    let choice' parsers =
        Seq.reduce alternative parsers

    let rec many1 (p : Parser<'a>) : Parser<'a seq> =
        parser {
            let! first = p
            let! rest = many p
            return seq { yield first; yield! rest }
        } 
    and many (p : Parser<'a>) : Parser<'a seq> =
        alternative (many1 p) (succeed Seq.empty)
        
    let leftOf (ign : Parser<unit>) (p : Parser<'a>) =
        build (fun a _ -> a)
        |> withParser p
        |> withParser ign

    [<DebuggerStepThrough>]
    let (<*) ign p = leftOf ign p

    let rightOf (ign : Parser<unit>) (p : Parser<'a>) =
        build (fun _ a -> a)
        |> withParser ign
        |> withParser p

    let between (left : Parser<unit>, right : Parser<unit>) (p : Parser<'a>) =
        build (fun _ a _ -> a)
        |> withParser left
        |> withParser p
        |> withParser right

    let chainLeft1 (operator : Parser<'a -> 'a -> 'a>) (operand : Parser<'a>) =
        let rec rest accum =
            choice [
                parser {
                    let! op = operator
                    let! value = operand
                    return! rest (op accum value)
                }
                succeed accum
            ]
        operand |> andThen rest

    [<DebuggerStepThrough>]
    let createForwardRef() : Parser<'a> * (Parser<'a> -> unit) =
        let mutable boxed = fail()
        let parser = fun input -> run boxed input
        (parser, fun p -> boxed <- p)