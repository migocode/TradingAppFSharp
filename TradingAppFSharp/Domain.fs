module Domain

open System
open DomainTypes
open DepotApiTypes
open ExternalStockApi

// GUI parameter
let private columnInnerSize = 15
let private numberOfPositionColumns = 6

type StockList = { isin: string; price: int }

let stockList =
    [ { isin = "Apple"; price = 467 }
      { isin = "Google"; price = 889 }
      { isin = "Microsoft"; price = 654 }
      { isin = "Facebook"; price = 234 }
      { isin = "Netflix"; price = 222 }
      { isin = "Amazon"; price = 993 }
      { isin = "Oracle"; price = 104 }
      { isin = "Intel"; price = 233 }
      { isin = "Erste"; price = 55 }
      { isin = "Voest"; price = 24 }
      { isin = "OMV"; price = 44 } ]

let existsInStockList (queryIsin: string) : bool =
    let isinExists =
        stockList
        |> List.map (fun stock -> stock.isin)
        |> List.exists (fun isin -> isin = queryIsin)

    if isinExists = false then
        printfn "The isin '%s' cannot be found in the stock list." queryIsin

    isinExists


let private sumAmount (values: Transaction list) : Amount =
    { value =
        values
        |> List.sumBy (fun v ->
            match v with
            | Buy v -> v.amount.value
            | Sell v -> -v.amount.value) }

let private sumCurrentValue (stock: Stock) (positions: Transaction list) : Currency =
    { value =
        (getCurrentPrice stock.isin).value
        * decimal (sumAmount positions).value }

let private sumTradingValue (positions: Transaction list) : Currency =
    { value =
        positions
        |> List.sumBy (fun p ->
            match p with
            | Buy p -> p.price.value * decimal p.amount.value
            | Sell p -> p.price.value * decimal -p.amount.value) }

let private aggregateStockPositions (stock: Stock, transactions: Transaction list) : Position =
    let currentValue = sumCurrentValue stock transactions
    let tradingValue = sumTradingValue transactions

    { currentValue = currentValue
      stock = stock
      differenceTotalInPercentage =
        { value =
            decimal (currentValue.value - tradingValue.value)
            * decimal 100
            / tradingValue.value }
      differenceTotalInCurrency = { value = currentValue.value - tradingValue.value }
      currentAmount = sumAmount transactions }

let private getPositions (depot: Depot) : Position list =
    depot.transactions
    |> List.groupBy (fun t ->
        match t with
        | Buy t -> t.stock
        | Sell t -> t.stock)
    |> List.map (aggregateStockPositions)

let private getPrintableTableRow (columnValues: string list) =
    let row =
        columnValues
        |> List.map (fun s -> $"| {s.PadRight columnInnerSize} ")
        |> List.reduce (fun a b -> a + b)

    $"{row} |"

let private getPositionHeader: string =
    getPrintableTableRow [ "ISIN"
                           "CurrentPrice"
                           "Amount"
                           "Current Value"
                           "Performance"
                           "Performance [%]" ]

let private getColumnsForPositionRow (p: Position) : string list =
    [ p.stock.isin.value
      string (getCurrentPrice p.stock.isin).value
      string p.currentAmount.value
      string p.currentValue.value
      string p.differenceTotalInCurrency.value
      string p.differenceTotalInPercentage.value ]

let private getPositionFromDepot (depot: Depot) (isin: Isin) : Option<SimplePosition> =
    match getPositions depot
          |> Seq.tryFind (fun p -> p.stock.isin = isin)
        with
    | Some x ->
        Some
            { currentAmount = x.currentAmount
              stock = x.stock }
    | None -> None

let private getAvailableAmountFromDepot (depot: Depot) (isin: Isin) : Amount =
    match getPositionFromDepot depot isin with
    | Some x -> x.currentAmount
    | None -> { value = 0 }

let private buyOrder (depot: Depot) (buy: MessageTypes.Buy) : Depot =

    let newTransaction =
        { stock =
            { isin = buy.isin
              name = getName buy.isin }
          amount = buy.buyAmount
          timestamp = DateTime.Now
          price = getCurrentPriceWhenTrading buy.isin }


    if existsInStockList buy.isin.value then
        { transactions = (Buy newTransaction) :: depot.transactions }
    else
        depot

let private sellOrder (depot: Depot) (sell: MessageTypes.Sell) : Depot =
    match
        getAvailableAmountFromDepot depot sell.isin < sell.sellAmount
        && not (existsInStockList sell.isin.value)
        with
    | true ->
        printfn "Sell order cannot be executed due to insufficient stock amount in the depot."
        depot
    | false ->
        let newTransaction =
            { stock =
                { isin = sell.isin
                  name = getName sell.isin }
              amount = sell.sellAmount
              timestamp = DateTime.Now
              price = getCurrentPriceWhenTrading sell.isin }

        { transactions = (Sell newTransaction) :: depot.transactions }

let private calcDepotValue (depot: Depot) : Depot =
    let depotValue =
        getPositions depot
        |> List.sumBy (fun t -> t.currentValue.value)

    printfn "Depot Value : %.2f" depotValue
    depot

let private printStocks (depot: Depot) : Depot =
    stockList
    |> List.iter (fun item -> printfn "isin: %A  price: %A" item.isin item.price)

    depot

let private printPositions (depot: Depot) : Depot =
    printfn $"{getPositionHeader}"

    printfn
        "%s"
        (String.replicate
            ((columnInnerSize + 3) * numberOfPositionColumns
             + 1)
            "-")

    getPositions depot
    |> List.map (fun p -> getColumnsForPositionRow p |> getPrintableTableRow)
    |> List.iter (fun p -> printfn $"{p}")

    depot

let depotApi: DepotApi =
    { buyOrder = buyOrder
      sellOrder = sellOrder
      calcDepotValue = calcDepotValue
      getPositions = printPositions
      getStocks = printStocks }
