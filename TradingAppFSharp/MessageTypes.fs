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

type Message =
    | Buy of Buy
    | Sell of Sell
    | DepotValue
    | DepotPositions