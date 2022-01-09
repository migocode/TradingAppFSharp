module Persistance

open DomainTypes
open MessageTypes
open System.Text.Json
open System.IO
open System

type PersistenceModel = 
    {
        transactionType: string
        stock: Stock
        amount: Amount
        timestamp: DateTime
        price: Currency
    }

let generatePersistenceModels (transactionInput: Transaction) : PersistenceModel = 
    match transactionInput with
    | Transaction.Buy x -> { 
        transactionType = "Buy"
        stock = x.stock
        amount = x.amount
        timestamp = x.timestamp
        price = x.price }
    | Transaction.Sell x -> {
        transactionType = "Sell"
        stock = x.stock
        amount = x.amount
        timestamp = x.timestamp
        price = x.price }

let generateDomainModels (transactionInput: PersistenceModel) : Transaction =
    match transactionInput.transactionType with
    | "Buy" -> Transaction.Buy {
            stock = transactionInput.stock
            amount = transactionInput.amount
            timestamp = transactionInput.timestamp
            price = transactionInput.price
        }
    | "Sell" -> Transaction.Buy {
            stock = transactionInput.stock
            amount = transactionInput.amount
            timestamp = transactionInput.timestamp
            price = transactionInput.price
        }
    | _ -> Transaction.Buy {
        stock = transactionInput.stock
        amount = { value = 0 }
        timestamp = transactionInput.timestamp
        price = transactionInput.price
    }


let persistDepot (depot: Depot) (fileName: string) : Depot = 
    let transactionsJson = JsonSerializer.Serialize (depot.transactions |> List.map generatePersistenceModels)
    File.WriteAllText(fileName, transactionsJson)
    depot

let loadDepot (depot: Depot) (fileName: string) : Depot = 
    let fileContent = File.ReadAllText fileName
    let transactions = (JsonSerializer.Deserialize<List<PersistenceModel>> fileContent) |> List.map generateDomainModels
    let newDepot = {
        transactions = transactions
    }
    newDepot

let managePersistence (depot: Depot) (msg: MessageTypes.PersistenceAction) : Depot =
    match msg with
    | Load fileName -> loadDepot depot fileName
    | Save fileName-> persistDepot depot fileName