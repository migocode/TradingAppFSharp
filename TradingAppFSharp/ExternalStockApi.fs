module ExternalStockApi

open Types

let getCurrentPrice (isin: string) : decimal =
    //let rnd = System.Random()
    //decimal (rnd.Next(0, 100))
    decimal 5

let getName (isin: Isin) : string =
    isin.value + "_name"
