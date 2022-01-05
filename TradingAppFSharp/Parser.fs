module Parser

open System
open MessageTypes

let safeEquals (it: string) (theOther: string) =
    String.Equals(it, theOther, StringComparison.OrdinalIgnoreCase)

[<Literal>]
let HelpLabel = "Help"

let (|Buy|Sell|Help|ParseFailed|DepotPositions|DepotValue|StockList|) (input: string) =
    let tryParseInt (arg: string) valueConstructor =
        let (worked, arg') = Int32.TryParse arg

        if worked then
            valueConstructor arg'
        else
            ParseFailed

    let parts = input.Split(' ') |> List.ofArray

    match parts with
    | [ verb; isin; amount ] when safeEquals verb (nameof Buy) ->
        tryParseInt amount (fun value ->
            Buy
                { buyAmount = { value = value }
                  messageTimestamp = DateTime.Now
                  isin = { value = isin } })
    | [ verb; isin; amount ] when safeEquals verb (nameof Sell) ->
        tryParseInt amount (fun value ->
            Sell
                { sellAmount = { value = value }
                  messageTimestamp = DateTime.Now
                  isin = { value = isin } })
    | [ verb ] when safeEquals verb (nameof DepotValue) -> DepotValue
    | [ verb ] when safeEquals verb (nameof DepotPositions) -> DepotPositions
    | [ verb ] when safeEquals verb (nameof StockList) -> StockList
    | [ verb ] when safeEquals verb HelpLabel -> Help
    | _ -> ParseFailed
