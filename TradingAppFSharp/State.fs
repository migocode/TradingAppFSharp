module State

open DomainTypes
open Domain

let init () : Depot = { transactions = List.empty }

let update (msg: MessageTypes.Message) (depot: Depot) : Depot =
    match msg with
    | MessageTypes.Message.Buy x -> depotApi.buyOrder depot x
    | MessageTypes.Message.Sell x -> depotApi.sellOrder depot x
    | MessageTypes.Message.DepotValue -> depotApi.calcDepotValue depot
    | MessageTypes.Message.DepotPositions -> depotApi.getPositions depot