namespace ParserComb

module Program =

    open System

    let spaceP : Parser<unit> =
        Parser.character Char.IsWhiteSpace
        |> Parser.many
        |> Parser.map ignore

    let trim p = Parser.leftOf spaceP p

    // Demo
    type Output = double 
    let zahlToOutput (zahl : double) : Output =
        zahl
    let add (l : Output) (r : Output) : Output =
        l + r
    let sub (l : Output) (r : Output) : Output =
        l - r
    let mul (l : Output) (r : Output) : Output =
        l * r
    let div (l : Output) (r : Output) : Output =
        l / r
    // 

    let zahlP : Parser<Output> =
        Parser.character Char.IsDigit
        |> Parser.many1
        |> Parser.map (Seq.toArray >> String)
        |> Parser.tryMap System.Double.TryParse
        |> Parser.map zahlToOutput
        |> trim

    let operatorP symbol op =
        Parser.exactChar symbol
        |> trim
        |> Parser.map (fun _ -> op)

    let addOps : Parser<Output -> Output -> Output> =
        Parser.choice
            [
                operatorP '+' add
                operatorP '-' sub
            ]
    let mulOps : Parser<Output -> Output -> Output> =
        Parser.choice
            [
                operatorP '*' mul 
                operatorP '/' div
            ]

    let bracedP (p : Parser<'a>) : Parser<'a> =
        Parser.between 
            ( Parser.exactChar '(' |> trim
            , Parser.exactChar ')' |> trim
            )
            p

    let expressionP : Parser<Output> =
        let (exprRef, setExpr) = Parser.createForwardRef()
        let factorP = Parser.alternative (bracedP exprRef) zahlP
        let termP   = factorP |> Parser.chainLeft1 mulOps
        let expP    = termP |> Parser.chainLeft1 addOps
        setExpr expP
        exprRef

    [<EntryPoint>]
    let main _ =
        let mutable cont = true
        while cont do
            Console.Write ("\n> ")
            let input = Console.ReadLine ()
            if input.ToLower().StartsWith("q") then cont <- false else
            match Parser.run expressionP input with
            | Success (output, _) -> Console.WriteLine output
            | Failure -> Console.WriteLine "parse error"
        0 // return an integer exit code