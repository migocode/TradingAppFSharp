module Domain

open System
open DomainTypes
open DepotApiTypes
open ExternalStockApi
open DomainTypeMappings

// GUI parameter
let private columnInnerSize = 15
let private numberOfPositionColumns = 6

let private sumAmount(values: AgnosticTransaction list) : Amount =
    { value = values |> List.sumBy(fun v -> v.amount.value) }

let private sumCurrentValue(stock: Stock) (positions: AgnosticTransaction list) : Currency =
    { value = (getCurrentPrice stock.isin).value *
                decimal (sumAmount positions).value }

let private sumTradingValue(positions: AgnosticTransaction list) : Currency =
    { value = positions |> List.sumBy(fun p -> p.price.value * decimal p.amount.value) }

let private aggregateStockPositions(stock: Stock, transactions: AgnosticTransaction list) : Position =
    let currentValue = sumCurrentValue stock transactions
    let tradingValue = sumTradingValue transactions
    { currentValue = currentValue
      stock = stock
      differenceTotalInPercentage = 
        { value = decimal (currentValue.value - tradingValue.value) * 
                  decimal 100 / tradingValue.value }
      differenceTotalInCurrency =
        { value = currentValue.value - tradingValue.value }
      currentAmount = sumAmount transactions }

let private getPositions (depot: Depot) : Position list =
    depot.transactions
    |> List.map(toAgnosticTransaction)
    |> List.groupBy(fun t -> t.stock)
    |> List.map(aggregateStockPositions)

let private getPrintableTableRow(columnValues: string list) =
    let row = columnValues
                |> List.map(fun s -> $"| {s.PadRight columnInnerSize} ")
                |> List.reduce(fun a b -> a + b)
    $"{row} |"

let private getPositionHeader : string =
    getPrintableTableRow
        ["ISIN"; 
         "CurrentPrice";
         "Amount";
         "Current Value";
         "Performance";
         "Performance [%]"]

let private getColumnsForPositionRow(p: Position) : string list =
    [p.stock.isin.value;
        string (getCurrentPrice p.stock.isin).value;
        string p.currentAmount.value;
        string p.currentValue.value;
        string p.differenceTotalInCurrency.value;
        string p.differenceTotalInPercentage.value]

let private getPositionFromDepot(depot: Depot) (isin: Isin) : Option<SimplePosition> =
    match getPositions depot |> Seq.tryFind(fun p -> p.stock.isin = isin) with
    | Some x -> Some { currentAmount = x.currentAmount
                       stock = x.stock}
    | None -> None

let private getAvailableAmountFromDepot (depot: Depot) (isin: Isin) : Amount =
    match getPositionFromDepot depot isin with
    | Some x -> x.currentAmount
    | None -> {value = 0}

let private buyOrder (depot: Depot) (buy: MessageTypes.Buy) : Depot =
    let newTransaction =
        { buyAmount = buy.buyAmount
          timestamp = DateTime.Now
          isin = buy.isin
          price = getCurrentPriceWhenTrading buy.isin }

    { transactions = (Buy newTransaction) :: depot.transactions }

let private sellOrder (depot: Depot) (sell: MessageTypes.Sell) : Depot =
    match getAvailableAmountFromDepot depot sell.isin < sell.sellAmount with
    | true -> 
        printfn "Sell order cannot be executed due to insufficient stock amount in the depot."
        depot
    | false ->
        let newTransaction =
            { sellAmount = sell.sellAmount
              timestamp = DateTime.Now
              isin = sell.isin
              price = getCurrentPriceWhenTrading sell.isin }

        { transactions = (Sell newTransaction) :: depot.transactions }

let private calcDepotValue (depot: Depot) : Depot =
    let depotValue =
        getPositions depot
        |> List.sumBy(fun t -> t.currentValue.value)

    printfn "Depot Value : %.2f" depotValue
    depot

let private printPositions (depot: Depot) : Depot =
    printfn $"{getPositionHeader}"
    printfn "%s" (String.replicate ((columnInnerSize + 3) * numberOfPositionColumns + 1) "-")
    getPositions depot
    |> List.map(fun p -> getColumnsForPositionRow p |> getPrintableTableRow)
    |> List.iter (fun p -> printfn $"{p}")
    depot

let depotApi: DepotApi =
    { buyOrder = buyOrder
      sellOrder = sellOrder
      calcDepotValue = calcDepotValue
      getPositions = printPositions }
