// --------------------------------------------------------------------------------------
// Copyright (c) Microsoft Corporation 2005-2011.
// This sample code is provided "as is" without warranty of any kind. 
// We disclaim all warranties, either express or implied, including the 
// warranties of merchantability and fitness for a particular purpose. 
// --------------------------------------------------------------------------------------

namespace FSharp.Data.Csv

open System
open System.IO

// --------------------------------------------------------------------------------------
// Runtime representation of CSV file
// --------------------------------------------------------------------------------------

/// Simple type that represents a single CSV row
type CsvRow internal (data:string[]) =
  member x.Columns = data

// Simple type wrapping CSV data
type CsvFile private (text:string) =
  // Cache the sequence of all data lines (all lines but the first)
  let lines = text.Split([| '\n'; '\r' |], StringSplitOptions.RemoveEmptyEntries)
  let lines =  [| for line in lines -> line.Split(',') |]
  let data = lines |> Seq.skip 1 |> Seq.map (fun d -> CsvRow(d)) |> Array.ofSeq
  member x.Data = data
  member x.Headers = lines |> Seq.head
  static member Parse(data) = new CsvFile(data)
#if PORTABLE
#else
  static member Load(path) = path |> File.ReadAllText |> CsvFile.Parse
#endif
