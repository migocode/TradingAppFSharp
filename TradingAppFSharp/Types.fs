module Types

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
