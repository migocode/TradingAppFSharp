module DomainTypes

open System

type Isin = { value: string }
type Amount = { value: int }
type Currency = { value: decimal }
type Percentage = { value: decimal }

type Stock = { isin: Isin; name: string }

type AgnosticTransaction =
    { stock: Stock
      amount: Amount
      price: Currency }

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

type Buy =
    { buyAmount: Amount
      timestamp: DateTime
      isin: Isin
      price: Currency }

type Sell =
    { sellAmount: Amount
      timestamp: DateTime
      isin: Isin
      price: Currency }

type Transaction =
    | Buy of Buy
    | Sell of Sell

type Depot = { transactions: List<Transaction> }
