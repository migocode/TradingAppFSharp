module DomainTypes

open System

type Isin = { value: string }
type Amount = { value: int }
type Currency = { value: decimal }
type Percentage = { value: decimal }

type Stock = { isin: Isin; name: string }

type SimplePosition =
    { currentAmount: Amount
      stock: Stock }

type Position =
    { currentValue: Currency
      stock: Stock
      //differenceYesterdayInPercent: Percentage
      //differenceYesterdayInCurrency: Currency
      differenceTotalInPercentage: Percentage
      differenceTotalInCurrency: Currency
      currentAmount: Amount }

type BaseTransaction =
    { stock: Stock
      amount: Amount
      timestamp: DateTime
      price: Currency }

type Transaction =
    | Buy of BaseTransaction
    | Sell of BaseTransaction

type Depot = { transactions: List<Transaction> }
