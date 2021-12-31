module Domain

open System
open Types
open ExternalStockApi

let init () : Depot = { transactions = List.empty }

module Implementation =

    let private dump(position: SimplePositionWithValue) : string =
        "Isin: " + position.stock.isin.value +
        ", Name: " + position.stock.name +
        ", Amount: " + string position.currentAmount.value +
        ", CurrentValue: " + string position.currentValueSum.value

    let private buyToPosition(buy: Buy) : SimplePosition =
        { currentAmount = buy.buyAmount
          stock = 
          { isin = buy.isin
            name = getName buy.isin } }

    let private sellToPosition(sell: Sell) : SimplePosition =
        { currentAmount = sell.sellAmount
          stock =
          { isin = sell.isin
            name = getName sell.isin } }

    let private transactionToPosition(transaction: Transaction) : SimplePosition =
        match transaction with
        | Buy x -> buyToPosition x
        | Sell x -> sellToPosition x

    let private sumAmount(values: SimplePosition list) : Amount =
        { value = values
                  |> List.sumBy(fun v -> v.currentAmount.value) }

    let private sumValue(stock: Stock) (positions: SimplePosition list) : Currency =
        { value = (getCurrentPrice stock.isin.value) *
                  decimal (positions
                           |> List.sumBy(fun v -> v.currentAmount.value)) }

    let private aggregateStockPositions(stock: Stock, positions: SimplePosition list) : SimplePositionWithValue  =
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

    let columnInnerSize = 15
    let numberOfPositionColumns = 6

    let private getTableRow(columnValues: string list) =
        let row = columnValues
                    |> List.map(fun s -> $"| {s.PadRight columnInnerSize} ")
                    |> List.reduce(fun a b -> a + b)
        $"{row} |"

    let private getPositionHeader =
        getTableRow
            ["ISIN"; 
             "CurrentPrice";
             "Amount";
             "Current Value";
             "Performance";
             "Performance [%]"]

    let private getColumnsForPositionRow(p: SimplePositionWithValue) =
        [p.stock.isin.value;
         string (getCurrentPrice p.stock.isin.value);
         string p.currentAmount.value;
         string p.currentValueSum.value;
         "todo: calc";
         "todo: calc"]

    let private printPositions (depot: Depot) =
        printfn $"{getPositionHeader}"
        printfn "%s" (String.replicate ((columnInnerSize + 3) * numberOfPositionColumns + 1) "-")
        getPositions depot
        |> List.map(fun p -> getColumnsForPositionRow p |> getTableRow)
        |> List.iter (fun p -> printfn $"{p}")
        depot

    let private buyOrder (depot: Depot) (buy: Buy) =
        let newTransaction =
            { buyAmount = buy.buyAmount
              timestamp = DateTime.Now
              isin = buy.isin }

        { transactions = (Buy newTransaction) :: depot.transactions }

    let private getPositionFromDepot(depot: Depot) (isin: Isin) : Option<SimplePositionWithValue> =
        getPositions depot |> Seq.tryFind(fun p -> p.stock.isin = isin)

    let private getAvailableAmountFromDepot (depot: Depot) (isin: Isin) : Amount =
        match getPositionFromDepot depot isin with
        | Some x -> x.currentAmount
        | None -> {value = 0}

    let private sellOrder (depot: Depot) (sell: Sell) =
        match getAvailableAmountFromDepot depot sell.isin < sell.sellAmount with
        | true -> 
            printf "Sell order cannot be executed due to insufficient amount of stocks in the depot."
            depot
        | false ->
            let newTransaction =
                { sellAmount = sell.sellAmount
                  timestamp = DateTime.Now
                  isin = sell.isin }

            { transactions = (Sell newTransaction) :: depot.transactions }

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
