module ExternalStockApi

open DomainTypes

let getCurrentPrice (isin: Isin) : Currency =
    //let rnd = System.Random()
    //decimal (rnd.Next(0, 100))
    match isin.value with
    | "Apple" -> { value = decimal 110 }
    | "Microsoft" -> { value = decimal 1200 }
    | _ -> { value = decimal 11 }

let getYesterdayPrice (isin: Isin) : Currency =
    //let rnd = System.Random()
    //decimal (rnd.Next(0, 100))
    match isin.value with
    | "Apple" -> { value = decimal 105 }
    | "Microsoft" -> { value = decimal 1050 }
    | _ -> { value = decimal 10.5 }

let getCurrentPriceWhenTrading (isin: Isin) : Currency =
    //let rnd = System.Random()
    //decimal (rnd.Next(0, 100))
    match isin.value with
    | "Apple" -> { value = decimal 100 }
    | "Microsoft" -> { value = decimal 1000 }
    | _ -> { value = decimal 10 }

let getName (isin: Isin) : string =
    isin.value + "_name"
