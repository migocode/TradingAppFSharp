module MessageTypes

open System
open DomainTypes

type Buy =
    { buyAmount: Amount
      messageTimestamp: DateTime
      isin: Isin }

type Sell =
    { sellAmount: Amount
      messageTimestamp: DateTime
      isin: Isin }

type PersistenceAction =
    | Load of string
    | Save of string

type Message =
    | Buy of Buy
    | Sell of Sell
    | DepotValue
    | DepotPositions
    | StockList
    | PersistenceAction of PersistenceAction
