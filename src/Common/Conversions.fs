// --------------------------------------------------------------------------------------
// Implements (runtime) conversions between primitive types
// --------------------------------------------------------------------------------------

module FSharp.Data.Conversions

open System
open System.Globalization

/// Convert the result of TryParse to option type
let asOption = function true, v -> Some v | _ -> None
    
let private (|StringEquals|_|) (s1:string) s2 = 
  if s1.Equals(s2, StringComparison.OrdinalIgnoreCase) 
    then Some () else None
    
type Operations =
  // Operations that convert string to supported primitive types
  static member ConvertString = Option.map (fun (s:string) -> s)
  static member ConvertDateTime = Option.bind (fun s -> 
    DateTime.TryParse(s, CultureInfo.InvariantCulture, DateTimeStyles.None) |> asOption)
  static member ConvertInteger = Option.bind (fun s -> 
      Int32.TryParse(s, NumberStyles.Any, CultureInfo.InvariantCulture) |> asOption)
  static member ConvertInteger64 = Option.bind (fun s -> 
      Int64.TryParse(s, NumberStyles.Any, CultureInfo.InvariantCulture) |> asOption)
  static member ConvertDecimal = Option.bind (fun s -> 
      Decimal.TryParse(s, NumberStyles.Any, CultureInfo.InvariantCulture) |> asOption)
  static member ConvertFloat = Option.bind (fun s -> 
      Double.TryParse(s, NumberStyles.Any, CultureInfo.InvariantCulture) |> asOption)
  static member ConvertBoolean = Option.bind (function 
      | StringEquals "true" | StringEquals "yes" -> Some true
      | StringEquals "false" | StringEquals "no" -> Some false
      | _ -> None)
    
  /// Operation that extracts the value from an option and reports a
  /// meaningful error message when the value is not there
  static member GetNonOptionalAttribute<'T>(name, opt:option<'T>) : 'T = 
      match opt with 
      | Some v -> v
      | None when typeof<'T> = typeof<string> -> Unchecked.defaultof<'T>
      | _ -> failwithf "Mismatch: %s is missing" name
