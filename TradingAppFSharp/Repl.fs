module Repl

open System
open Parser

type Message =
    | DomainMessage of Domain.Message
    | HelpRequested
    | NotParsable of string

let read (input: string) =
    match input with
    | Buy v -> Domain.Buy v |> DomainMessage
    | Sell v -> Domain.Sell v |> DomainMessage
    | DepotPositions -> Domain.DepotPositions |> DomainMessage
    | DepotValue -> Domain.DepotValue |> DomainMessage
    | Help -> HelpRequested
    | ParseFailed -> NotParsable input

open Microsoft.FSharp.Reflection

let createHelpText () : string =
    FSharpType.GetUnionCases typeof<Domain.Message>
    |> Array.map (fun case -> case.Name)
    |> Array.fold (fun prev curr -> prev + " " + curr) ""
    |> (fun s -> s.Trim() |> sprintf "Known commands are: %s")

let evaluate (update: Domain.Message -> Domain.Depot -> Domain.Depot) (depot: Domain.Depot) (msg: Message) =
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

let print (state: Domain.Depot, outputToPrint: string) =
    printfn "%s\n" outputToPrint
    printf "> "

    state

let rec loop (depot: Domain.Depot) =
    Console.ReadLine()
    |> read
    |> evaluate Domain.update depot
    |> print
    |> loop
