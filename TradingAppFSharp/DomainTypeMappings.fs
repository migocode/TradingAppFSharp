module DomainTypeMappings

open DomainTypes
open ExternalStockApi

let private buyToAgnosticTransaction(buy: Buy) : AgnosticTransaction =
    { stock =
        { isin = buy.isin
          name = getName buy.isin }
      amount = buy.buyAmount
      price = buy.price }

let private sellToAgnosticTransaction(sell: Sell) : AgnosticTransaction =
    { stock =
        { isin = sell.isin
          name = getName sell.isin }
      amount = { value = -sell.sellAmount.value }
      price = sell.price }

let toAgnosticTransaction(transaction: Transaction) : AgnosticTransaction =
   match transaction with
    | Buy x -> buyToAgnosticTransaction x
    | Sell x -> sellToAgnosticTransaction x
