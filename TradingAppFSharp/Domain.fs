module Domain

open System

type Isin = { value: string }
type Amount = { value: int }
type Currency = { value: decimal }
type Percentage = { value: decimal }

type Buy = {
    buyAmount: Amount; 
    timestamp: DateTime; 
    isin: Isin
}

type Sell = {
    sellAmount: Amount; 
    timestamp: DateTime; 
    isin: Isin
}

type Message =
    | Buy of Buy
    | Sell of Sell
    | DepotValue
    | DepotPositions

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

let getCurrentPrice (isin: string) : decimal =
    let rnd = System.Random()
    rnd.Next(0, 100)

module Implementation =
    let private buyOrder (depot: Depot) (buy: Buy) =
        let newTransaction = { buyAmount = buy.buyAmount; timestamp = DateTime.Now; isin = buy.isin }
        { transactions = (Buy newTransaction) :: depot.transactions }

    let private sellOrder (depot: Depot) (sell: Sell) =
        let newTransaction = { sellAmount = sell.sellAmount; timestamp = DateTime.Now; isin = sell.isin }
        { transactions = (Sell newTransaction) :: depot.transactions }

    let private calcDepotValue (depot: Depot) : Currency =
        {
            value =
                depot.transactions
                |> List.map (fun t -> match t with
                    | Sell s -> (decimal) s.sellAmount.value * getCurrentPrice s.isin.value
                    | Buy b -> (decimal) b.buyAmount.value  * getCurrentPrice b.isin.value)
                |> List.sum
        }

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
    | Message.Buy x -> Implementation.depotApi.buyOrder depot x
    | Message.Sell x -> depot
    | Message.DepotValue -> depot //TODO: and print value
    | Message.DepotPositions -> depot //TODO: and print depot positions