module ExternalStockApi

open DomainTypes

let getCurrentPrice (isin: Isin) : Currency =
    //let rnd = System.Random()
    //decimal (rnd.Next(0, 100))
    { value = decimal 11 }

let getCurrentPriceWhenTrading (isin: Isin) : Currency =
    //let rnd = System.Random()
    //decimal (rnd.Next(0, 100))
    { value = decimal 10 }

let getName (isin: Isin) : string =
    isin.value + "_name"
