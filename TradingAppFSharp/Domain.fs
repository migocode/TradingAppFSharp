module Domain

open System

type State = int

type Message =
    | Buy of int
    | Sell of int

type Isin = { value: string }

type Amount = { value: string }

type Currency = { value: decimal }

type Percentage = { value: decimal }

type Transaction = { amount: Amount; price: Currency; timestamp: DateTime; isin: Isin}

type Stock = { isin: Isin; name: string }

type Depot = { constituents: List<Transaction> }

let init () : State =
    0

let update (msg : Message) (model : State) : State =
    match msg with
    | Buy x -> model + x
    | Sell x -> model - x