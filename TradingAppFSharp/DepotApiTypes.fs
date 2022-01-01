module DepotApiTypes

open DomainTypes

type DepotApi =
    { buyOrder: Depot -> MessageTypes.Buy -> Depot
      sellOrder: Depot -> MessageTypes.Sell -> Depot
      calcDepotValue: Depot -> Depot
      getPositions: Depot -> Depot }
