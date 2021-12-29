module Domain

open System

type Isin = { value: string }
type Amount = { value: string }
type Currency = { value: decimal }
type Percentage = { value: decimal }


type Buy = {
    amount: Amount; 
    timestamp: DateTime; 
    isin: Isin
}

type Sell = {
    amount: Amount; 
    timestamp: DateTime; 
    isin: Isin
}

type Message =
    | Buy of Buy
    | Sell of Sell

type Transaction = 
    | Buy of Buy
    | Sell of Sell

type Stock = { isin: Isin; name: string }

type Depot = { 
    transactions: List<Transaction> 
}

type Position = {
    currentValue: Currency;
    stock: Stock;
    differenceYesterdayInPercent: Percentage;
    differenceYesterdayInCurrency: Currency;
    differenceTotalInPercentage: Percentage;
    differenceTotalInCurrency: Currency;
    currentAmount: Amount
}

type DepotApi = {
    buyOrder: Depot -> Buy -> Depot
    sellOrder: Depot -> Sell -> Depot
    calcDepotValue: Depot -> Currency
    getPositions: Depot -> List<Position>
}

let init () : Depot = { transactions = List.empty }

module Implementation =
    let private buyOrder (depot: Depot) (buy: Buy) =
        let newTransaction = { amount = buy.amount; price = { value = 0M }; timestamp = DateTime.Now; isin = buy.isin }
        { transactions = newTransaction :: depot.transactions }

    let private sellOrder (depot: Depot) (sell: Sell) =
        let newTransaction = { amount = sell.amount; price = { value = 0M }; timestamp = DateTime.Now; isin = sell.isin }
        { transactions = newTransaction :: depot.transactions }

    let private calcDepotValue (depot: Depot) : Currency =
        { value = depot.transactions |> List.map (fun t -> t.price.value) |> List.sum }

    let private getPositions (depot: Depot) =
        List.empty

    let depotApi: DepotApi = {
        buyOrder = buyOrder
        sellOrder = sellOrder
        calcDepotValue = calcDepotValue
        getPositions = getPositions
    }

let update (msg : Message) (depot : Depot) : Depot =
    match msg with
    | Buy x -> Implementation.depotApi.buyOrder depot x
    | Sell x -> depot