module Domain

open System
open DomainTypes
open DepotApiTypes
open ExternalStockApi
open Persistance

let private placeholderSumPosition = { isin = { value = "Sum" }; name = "Sum"}

let private safeDiv (a: decimal) (b: decimal) : decimal =
    if b.Equals Decimal.Zero then decimal 99999999 else a / b

let existsInStockList (stockName: string) : bool =
    let isinExists =
        stockList
        |> List.map (fun stock -> stock.name)
        |> List.exists (fun name -> name = stockName)

    if isinExists = false then
        printfn "<<<< The isin '%s' cannot be found in the stock list. >>>>" stockName

    isinExists

let private sumAmount (values: Transaction list) : Amount =
    { value =
        values
        |> List.sumBy (fun v ->
            match v with
            | Buy v -> v.amount.value
            | Sell v -> -v.amount.value) }

let private sumCurrentValue (stock: Stock, positions: Transaction list) : Currency =
    { value =
        (getCurrentPrice stock.isin).value
        * decimal (sumAmount positions).value }

let private sumYesterdayValue (stock: Stock, positions: Transaction list) : Currency =
    { value = (getYesterdayPrice stock.isin).value *
                decimal (sumAmount positions).value }

let private sumTradingValue (positions: Transaction list) : Currency =
    { value =
        positions
        |> List.sumBy (fun p ->
            match p with
            | Buy p -> p.price.value * decimal p.amount.value
            | Sell p -> p.price.value * decimal -p.amount.value) }

let private calculatePosition (stock: Stock)
                              (currentValue: Currency)
                              (yesterdayValue: Currency)
                              (tradingValue: Currency)
                              (transactions: Transaction list)
                              : Position =

    { currentValue = currentValue
      stock = stock
      differenceTotalInPercentage = 
        { value = decimal (currentValue.value - tradingValue.value) *
                  safeDiv (decimal 100) tradingValue.value }
      differenceTotalInCurrency =
        { value = currentValue.value - tradingValue.value }
      currentAmount = sumAmount transactions
      differenceYesterdayInCurrency = 
        { value = currentValue.value - yesterdayValue.value }
      differenceYesterdayInPercent =
        { value = decimal (currentValue.value - yesterdayValue.value) *
                  safeDiv (decimal 100) yesterdayValue.value } }

let private aggregateStockPositions (stock: Stock, transactions: Transaction list) : Position =
    let currentValue = sumCurrentValue (stock, transactions)
    let yesterdayValue = sumYesterdayValue (stock, transactions)
    let tradingValue = sumTradingValue transactions
    calculatePosition stock currentValue yesterdayValue tradingValue transactions

let private getTransactionStock (t: Transaction) : Stock =
    match t with
    | Buy t -> t.stock
    | Sell t -> t.stock

let private getPositions (depot: Depot) : Position list =
    depot.transactions
    |> List.groupBy getTransactionStock
    |> List.map aggregateStockPositions

let private calculateValue (calculationFun: Stock * Transaction list -> Currency)
                           (transactionsForStocks: (Stock * Transaction list) list)
                           : Currency =

    { Currency.value =
        transactionsForStocks
        |> List.map(calculationFun)
        |> List.sumBy(fun t -> t.value) }

let private getDepotSum (depot: Depot) : Position =
    let transactionsForStocks = depot.transactions |> List.groupBy(getTransactionStock)
    let currentValue = calculateValue sumCurrentValue transactionsForStocks
    let yesterdayValue = calculateValue sumYesterdayValue transactionsForStocks
    let tradingValue = sumTradingValue depot.transactions
    calculatePosition placeholderSumPosition currentValue yesterdayValue tradingValue depot.transactions

let private getPrintableTableRow (columnValues: string list) =
    let row =
        columnValues
        |> List.map (fun s -> $"| {s.PadRight Const.Gui.columnInnerSize} ")
        |> List.reduce (fun a b -> a + b)

    $"{row} |"

let private positionHeaderColumns =
    ["ISIN";
    "CurrentPrice";
    "Amount";
    "Current Value";
    "Performance";
    "Performance [%]";
    "Performance (y.)";
    "Performance [%] (y.)"]

let private getPositionHeader () : string =
    getPrintableTableRow positionHeaderColumns

let private getColumnsForPositionRow (p: Position) : string list =
    [p.stock.isin.value;
     if p.stock.isin.value = Const.Gui.sumRowName
     then "" else $"%.2f{(getCurrentPrice p.stock.isin).value}";
     string p.currentAmount.value;
     $"%.2f{p.currentValue.value}";
     $"%.2f{p.differenceTotalInCurrency.value}";
     $"%.2f{p.differenceTotalInPercentage.value}";
     $"%.2f{p.differenceYesterdayInCurrency.value}";
     $"%.2f{p.differenceYesterdayInPercent.value}"]

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
        || not (existsInStockList sell.isin.value)
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

let private printLine () =
    printfn "%s" (String.replicate ((Const.Gui.columnInnerSize + 3) * positionHeaderColumns.Length + 2) "-")

let private printHeader () =
    printfn $"{getPositionHeader ()}"
    printLine ()

let private printDepotSum (depot: Depot) =
    printLine ()

    printfn
        $"{getDepotSum depot
           |> getColumnsForPositionRow
           |> getPrintableTableRow}"

let private printPositions (depot: Depot) =
    getPositions depot
    |> List.map (fun p -> getColumnsForPositionRow p |> getPrintableTableRow)
    |> List.iter (fun p -> printfn $"{p}")

let private printDepot (depot: Depot) : Depot =
    printHeader ()
    printPositions depot
    printDepotSum depot
    depot

let private printStocks (depot: Depot) : Depot =
    stockList
    |> List.iter (fun item -> printfn "isin: %s  price: %.2f" item.isin.value (getCurrentPrice item.isin).value)
    depot

let depotApi: DepotApi =
    { buyOrder = buyOrder
      sellOrder = sellOrder
      calcDepotValue = calcDepotValue
      getPositions = printDepot
      getStocks = printStocks 
      managePersistence = managePersistence}
