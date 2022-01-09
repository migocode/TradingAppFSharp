module Parser

open System
open MessageTypes

let safeEquals (it: string) (theOther: string) =
    String.Equals(it, theOther, StringComparison.OrdinalIgnoreCase)

[<Literal>]
let HelpLabel = "Help"

let (|Buy|Sell|PersistenceAction|ParseFailed|DepotPositions|DepotValue|StockList|) (input: string) =  
    let (|Load|Save|ParseFailed|) (input: string) =
        match input with
        | action when safeEquals action (nameof Load) ->
            Load
        | action when safeEquals action (nameof Save) ->
            Save
        | _ -> ParseFailed
    
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
    | [ verb; action; fileName ] when safeEquals verb (nameof PersistenceAction) -> 
        match action with
        | Load -> PersistenceAction (PersistenceAction.Load fileName)
        | Save -> PersistenceAction (PersistenceAction.Save fileName)
        | _ -> ParseFailed
    | _ -> ParseFailed
