module Repl

open System
open Parser

type Message =
    | DomainMessage of MessageTypes.Message
    | HelpRequested
    | NotParsable of string

let read (input: string) =
    match input with
    | Buy v -> MessageTypes.Buy v |> DomainMessage
    | Sell v -> MessageTypes.Sell v |> DomainMessage
    | DepotPositions -> MessageTypes.DepotPositions |> DomainMessage
    | DepotValue -> MessageTypes.DepotValue |> DomainMessage
    | StockList -> MessageTypes.StockList |> DomainMessage
    | PersistenceAction v -> MessageTypes.PersistenceAction v |> DomainMessage
    | ParseFailed -> NotParsable input

open Microsoft.FSharp.Reflection

let createHelpText () : string =
    FSharpType.GetUnionCases typeof<MessageTypes.Message>
    |> Array.map (fun case -> case.Name)
    |> Array.fold (fun prev curr -> prev + " " + curr) ""
    |> (fun s -> s.Trim() |> sprintf "Known commands are: %s")

let evaluate
    (update: MessageTypes.Message -> DomainTypes.Depot -> DomainTypes.Depot)
    (depot: DomainTypes.Depot)
    (msg: Message) =
    match msg with
    | DomainMessage msg ->
        let newState = update msg depot

        let message =
            sprintf "The message was %A. \n \n New state is %A  \n \n" msg newState

        (newState, message)
    | HelpRequested ->
        let message = createHelpText ()
        (depot, message)
    | NotParsable originalInput ->
        let message =
            sprintf """"%s" was not parsable.""" originalInput

        (depot, message)

let print (state: DomainTypes.Depot, outputToPrint: string) =
    printfn "%s\n" outputToPrint
    printf "> "
    state

let rec loop (depot: DomainTypes.Depot) =
    Console.ReadLine()
    |> read
    |> evaluate State.update depot
    |> print
    |> loop
