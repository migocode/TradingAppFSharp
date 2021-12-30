module Domain

open System

type Isin = { value: string }
type Amount = { value: int }
type Currency = { value: decimal }
type Percentage = { value: decimal }

type Buy =
    { buyAmount: Amount
      timestamp: DateTime
      isin: Isin }

type Sell =
    { sellAmount: Amount
      timestamp: DateTime
      isin: Isin }

type Message =
    | Buy of Buy
    | Sell of Sell
    | DepotValue
    | DepotPositions

type Transaction =
    | Buy of Buy
    | Sell of Sell

type Stock = { isin: Isin; name: string }

type Depot = { transactions: List<Transaction> }

type SimplePosition =
    {
        currentAmount: Amount
        stock: Stock
    }

type SimplePositionWithValue =
    {
        currentAmount: Amount
        currentValueSum: Currency
        stock: Stock
    }

type Position =
    { currentValue: Currency
      stock: Stock
      //differenceYesterdayInPercent: Percentage
      //differenceYesterdayInCurrency: Currency
      differenceTotalInPercentage: Percentage
      differenceTotalInCurrency: Currency
      currentAmount: Amount }

type DepotApi =
    { buyOrder: Depot -> Buy -> Depot
      sellOrder: Depot -> Sell -> Depot
      calcDepotValue: Depot -> Depot
      getPositions: Depot -> Depot }

let init () : Depot = { transactions = List.empty }

let getCurrentPrice (isin: string) : decimal =
    //let rnd = System.Random()
    //decimal (rnd.Next(0, 100))
    decimal 5

let getName (isin: Isin) : string =
    isin.value + "_name"

module Implementation =
    let private buyOrder (depot: Depot) (buy: Buy) =
        let newTransaction =
            { buyAmount = buy.buyAmount
              timestamp = DateTime.Now
              isin = buy.isin }

        { transactions = (Buy newTransaction) :: depot.transactions }

    let private sellOrder (depot: Depot) (sell: Sell) =
        let newTransaction =
            { sellAmount = sell.sellAmount
              timestamp = DateTime.Now
              isin = sell.isin }

        { transactions = (Sell newTransaction) :: depot.transactions }

    let private dump(position: SimplePositionWithValue) : string =
        "Isin: " + position.stock.isin.value +
        ", Name: " + position.stock.name +
        ", Amount: " + string position.currentAmount.value +
        ", CurrentValue: " + string position.currentValueSum.value

    let buyToPosition(buy: Buy) : SimplePosition =
        { currentAmount = buy.buyAmount
          stock = 
          { isin = buy.isin
            name = getName buy.isin } }

    let sellToPosition(sell: Sell) : SimplePosition =
        { currentAmount = sell.sellAmount
          stock =
          { isin = sell.isin
            name = getName sell.isin } }

    let transactionToPosition(transaction: Transaction) : SimplePosition =
        match transaction with
        | Buy x -> buyToPosition x
        | Sell x -> sellToPosition x

    let sumAmount(values: SimplePosition list) : Amount =
        { value = values
                  |> List.sumBy(fun v -> v.currentAmount.value) }

    let sumValue(stock: Stock) (positions: SimplePosition list) : Currency =
        { value = (getCurrentPrice stock.isin.value) *
                  decimal (positions
                           |> List.sumBy(fun v -> v.currentAmount.value)) }

    let aggregateStockPositions(stock: Stock, positions: SimplePosition list) : SimplePositionWithValue  =
        { currentAmount = sumAmount positions
          currentValueSum = sumValue stock positions
          stock = stock }

    let private getPositions (depot: Depot) =
        depot.transactions
        |> List.map(transactionToPosition)
        |> List.groupBy(fun t -> t.stock)
        |> List.map(aggregateStockPositions)

    let private calcDepotValue (depot: Depot) : Depot =
        let depotValue =
            getPositions depot
            |> List.sumBy(fun t -> t.currentValueSum.value)

        printfn "Depot_Value : %A" depotValue
        depot

    let printPositions (depot: Depot) =
        getPositions depot
        |> List.map(dump)
        |> List.iter (printfn "%s")
        depot

    let depotApi: DepotApi =
        { buyOrder = buyOrder
          sellOrder = sellOrder
          calcDepotValue = calcDepotValue
          getPositions = printPositions }

let update (msg: Message) (depot: Depot) =
    match msg with
    | Message.Buy x -> Implementation.depotApi.buyOrder depot x
    | Message.Sell x -> Implementation.depotApi.sellOrder depot x
    | Message.DepotValue -> Implementation.depotApi.calcDepotValue depot
    | Message.DepotPositions -> Implementation.depotApi.getPositions depot
