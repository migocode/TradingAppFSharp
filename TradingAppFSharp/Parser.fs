module Parser

open System
open Types

let safeEquals (it: string) (theOther: string) =
    String.Equals(it, theOther, StringComparison.OrdinalIgnoreCase)

[<Literal>]
let HelpLabel = "Help"

let (|Buy|Sell|Help|ParseFailed|DepotPositions|DepotValue|) (input: string) =
    let tryParseInt (arg: string) valueConstructor =
        let (worked, arg') = Int32.TryParse arg

        if worked then
            valueConstructor arg'
        else
            ParseFailed

    let parts = input.Split(' ') |> List.ofArray

    match parts with
    | [ verb; isin; amount ] when safeEquals verb (nameof Types.Buy) ->
        tryParseInt amount (fun value ->
            Buy
                { buyAmount = { value = value }
                  timestamp = DateTime.Now
                  isin = { value = isin } })
    | [ verb; isin; amount ] when safeEquals verb (nameof Types.Sell) ->
        tryParseInt amount (fun value ->
            Sell
                { sellAmount = { value = -value }
                  timestamp = DateTime.Now
                  isin = { value = isin } })
    | [ verb ] when safeEquals verb (nameof Types.DepotValue) -> DepotValue
    | [ verb ] when safeEquals verb (nameof Types.DepotPositions) -> DepotPositions
    | [ verb ] when safeEquals verb HelpLabel -> Help
    | _ -> ParseFailed
