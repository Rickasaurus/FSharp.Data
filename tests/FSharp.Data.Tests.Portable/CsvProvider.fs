module FSharp.Data.Portable.Tests

open System
open System.IO
open System.Net
open FSharp.Data

type Stocks = CsvProvider<"../../samples/docs/MSFT.csv">

let test () =
  let msft = Stocks.Parse("""Date,Open,High,Low,Close,Volume,Adj Close
  2012-01-27,29.45,29.53,29.17,29.23,44187700,29.23
  2012-01-26,29.61,29.70,29.40,29.50,49102800,29.50
  2012-01-25,29.07,29.65,29.07,29.56,59231700,29.56
  """)


  (*
  let download () = async {
    // Download the stock prices
    let uri = Uri("http://ichart.finance.yahoo.com/table.csv?s=MSFT")
    let request = HttpWebRequest.CreateHttp(uri)
    let! response = request.AsyncGetResponse()
    let stream = response.GetResponseStream()
    let reader = new StreamReader(stream)
    let data = reader.ReadToEnd()
    return Stocks.Parse(data) }

  let test () = async {
    let! msft = download() 
  *)

  // Look at the most recent row. Note the 'Date' property
  // is of type 'DateTime' and 'Open' has a type 'decimal'
  let firstRow = msft.Data |> Seq.head
  let lastDate = firstRow.Date
  let lastOpen = firstRow.Open

  // Print the prices in the HLOC format
  for row in msft.Data do
    printfn "HLOC: (%A, %A, %A, %A)" row.High row.Low row.Open row.Close