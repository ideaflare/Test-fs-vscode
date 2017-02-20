// #load "LunoDotNET.fs"
#reference "../packages/FSharp.Data/lib/net40/FSharp.Data.dll"

// open LunoDotNET
open FSharp.Data

let tickerUrl = "https://api.mybitx.com/api/1/ticker?pair=XBTZAR"

type Ticker = JsonProvider<"https://api.mybitx.com/api/1/ticker?pair=XBTZAR">

let ticker = Ticker.Load(tickerUrl)

let tickerDate = System.DateTimeOffset.FromUnixTimeMilliseconds(ticker.Timestamp).UtcDateTime