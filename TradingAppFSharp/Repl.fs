module Repl

open System
open Parser

type Message =
    | DomainMessage of Types.Message
    | HelpRequested
    | NotParsable of string

let read (input: string) =
    match input with
    | Buy v -> Types.Buy v |> DomainMessage
    | Sell v -> Types.Sell v |> DomainMessage
    | DepotPositions -> Types.DepotPositions |> DomainMessage
    | DepotValue -> Types.DepotValue |> DomainMessage
    | Help -> HelpRequested
    | ParseFailed -> NotParsable input

open Microsoft.FSharp.Reflection

let createHelpText () : string =
    FSharpType.GetUnionCases typeof<Types.Message>
    |> Array.map (fun case -> case.Name)
    |> Array.fold (fun prev curr -> prev + " " + curr) ""
    |> (fun s -> s.Trim() |> sprintf "Known commands are: %s")

let evaluate (update: Types.Message -> Types.Depot -> Types.Depot) (depot: Types.Depot) (msg: Message) =
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
            sprintf
                """"%s" was not parsable. %s"""
                originalInput
                "You can get information about known commands by typing \"Help\""

        (depot, message)

let print (state: Types.Depot, outputToPrint: string) =
    printfn "%s\n" outputToPrint
    printf "> "

    state

let rec loop (depot: Types.Depot) =
    Console.ReadLine()
    |> read
    |> evaluate Domain.update depot
    |> print
    |> loop
