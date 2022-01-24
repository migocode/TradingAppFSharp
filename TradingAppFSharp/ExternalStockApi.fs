module ExternalStockApi

open DomainTypes

let getCurrentPrice (isin: Isin) : Currency =
    match isin.value with
    | "Apple" -> { value = decimal 100 }
    | "Microsoft" -> { value = decimal 200 }
    | "Google" -> { value = decimal 300 }
    | "Facebook" -> { value = decimal 400 }
    | "Netflix" -> { value = decimal 500 }
    | "Amazon" -> { value = decimal 600 }
    | "Oracle" -> { value = decimal 700 }
    | "Intel" -> { value = decimal 800 }
    | "Erste" -> { value = decimal 900 }
    | "Voest" -> { value = decimal 1000 }
    | "OMV" -> { value = decimal 1100 }
    | _ -> { value = decimal 10 }

let getYesterdayPrice (isin: Isin) : Currency =
    match isin.value with
    | "Apple" -> { value = decimal 1.60 }
    | "Microsoft" -> { value = decimal 20.7 }
    | "Google" -> { value = decimal 30.4 }
    | "Facebook" -> { value = decimal 4.00 }
    | "Netflix" -> { value = decimal 50.9 }
    | "Amazon" -> { value = decimal 600 }
    | "Oracle" -> { value = decimal 7.80 }
    | "Intel" -> { value = decimal 80.5 }
    | "Erste" -> { value = decimal 90.6 }
    | "Voest" -> { value = decimal 10.30 }
    | "OMV" -> { value = decimal 11.50 }
    | _ -> { value = decimal 10 }

let getCurrentPriceWhenTrading (isin: Isin) : Currency =
    match isin.value with
    | "Apple" -> { value = decimal 10.60 }
    | "Microsoft" -> { value = decimal 10.7 }
    | "Google" -> { value = decimal 20.4 }
    | "Facebook" -> { value = decimal 40.00 }
    | "Netflix" -> { value = decimal 500.9 }
    | "Amazon" -> { value = decimal 610 }
    | "Oracle" -> { value = decimal 71.80 }
    | "Intel" -> { value = decimal 8.5 }
    | "Erste" -> { value = decimal 9.6 }
    | "Voest" -> { value = decimal 15.30 }
    | "OMV" -> { value = decimal 59.50 }
    | _ -> { value = decimal 10 }

let getName (isin: Isin) : string =
    isin.value

let stockList =
    [ { name = "Apple"; isin = { value = "Apple"} }
      { name = "Google"; isin = { value = "Google"} }
      { name = "Microsoft"; isin = { value = "Microsoft"} }
      { name = "Facebook"; isin = { value = "Facebook"} }
      { name = "Netflix"; isin = { value = "Netflix"} }
      { name = "Amazon"; isin = { value = "Amazon"} }
      { name = "Oracle"; isin = { value = "Oracle"} }
      { name = "Intel"; isin = { value = "Intel"} }
      { name = "Erste"; isin = { value = "Erste"} }
      { name = "Voest"; isin = { value = "Voest"} }
      { name = "OMV"; isin = { value = "OMV"} } 
      ]