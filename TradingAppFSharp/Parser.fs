module Parser

open System
open Domain

let safeEquals (it : string) (theOther : string) =
    String.Equals(it, theOther, StringComparison.OrdinalIgnoreCase)

[<Literal>]
let HelpLabel = "Help"

let (|Buy|Sell|Help|ParseFailed|) (input : string) =
    let tryParseInt (arg : string) valueConstructor =
        let (worked, arg') = Int32.TryParse arg
        if worked then valueConstructor arg' else ParseFailed

    let parts = input.Split(' ') |> List.ofArray
    match parts with
    | [ verb; isin; amount ] when safeEquals verb (nameof Domain.Buy) -> 
            tryParseInt amount (fun value -> Buy { buyAmount = { value = amount }; timestamp = DateTime.Now; isin = { value = isin } })
    | [ verb; isin; amount ] when safeEquals verb (nameof Domain.Sell) ->
            tryParseInt amount (fun value -> Sell { sellAmount = { value = amount }; timestamp = DateTime.Now; isin = { value = isin } })
    | [ verb ] when safeEquals verb HelpLabel -> Help
    | _ -> ParseFailed