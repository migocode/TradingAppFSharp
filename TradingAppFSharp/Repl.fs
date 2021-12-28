module Repl

open System
open Parser
open Domain

type Message =
    | DomainMessage of Domain.Message
    | HelpRequested
    | NotParsable of string

//let read (input : string) =
//    match input with
//    | Increment -> Domain.Increment |> DomainMessage
//    | Decrement -> Domain.Decrement |> DomainMessage
//    | IncrementBy v -> Domain.IncrementBy v |> DomainMessage
//    | DecrementBy v -> Domain.DecrementBy v |> DomainMessage
//    | Help -> HelpRequested
//    | ParseFailed  -> NotParsable input

let read (input : string) = Buy

open Microsoft.FSharp.Reflection

let createHelpText () : string =
    FSharpType.GetUnionCases typeof<Domain.Message>
    |> Array.map (fun case -> case.Name)
    |> Array.fold (fun prev curr -> prev + " " + curr) ""
    |> (fun s -> s.Trim() |> sprintf "Known commands are: %s")

let evaluate (update : Domain.Message -> Depot -> Depot) (depot : Depot) (msg : Message) =
    match msg with
    | DomainMessage msg ->
        let newState = update msg depot
        let message = sprintf "The message was %A. New state is %A" msg newState
        (newState, message)
    | HelpRequested ->
        let message = createHelpText ()
        (depot, message)
    | NotParsable originalInput ->
        let message =
            sprintf """"%s" was not parsable. %s"""  originalInput "You can get information about known commands by typing \"Help\""
        (depot, message)

let print (state : Depot, outputToPrint : string) =
    printfn "%s\n" outputToPrint
    printf "> "

    state

let rec loop (depot : Depot) =
    Console.ReadLine()
    |> read
    |> evaluate Domain.update depot
    |> print
    |> loop