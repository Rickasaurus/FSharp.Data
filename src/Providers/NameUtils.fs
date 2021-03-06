﻿// --------------------------------------------------------------------------------------
// Tools for generating nice member names that follow F# & .NET naming conventions
// --------------------------------------------------------------------------------------

module ProviderImplementation.NameUtils

open System
open System.Globalization
open System.Collections.Generic

// --------------------------------------------------------------------------------------
// Active patterns & operators for parsing strings

let private tryAt (s:string) i = if i >= s.Length then None else Some s.[i]
let private sat f (c:option<char>) = match c with Some c when f c -> Some c | _ -> None
let private (|EOF|_|) c = match c with Some _ -> None | _ -> Some ()
let private (|LetterDigit|_|) = sat Char.IsLetterOrDigit
let private (|Upper|_|) = sat Char.IsUpper
let private (|Lower|_|) = sat Char.IsLower

// --------------------------------------------------------------------------------------

/// Turns a given non-empty string into a nice 'PascalCase' identifier
let nicePascalName (s:string) = 
  // Starting to parse a new segment 
  let rec restart i = seq {
    match tryAt s i with 
    | EOF -> ()
    | LetterDigit _ & Upper _ -> yield! upperStart i (i + 1)
    | LetterDigit _ -> yield! consume i false (i + 1)
    | _ -> yield! restart (i + 1) }
  // Parsed first upper case letter, continue either all lower or all upper
  and upperStart from i = seq {
    match tryAt s i with 
    | Upper _ -> yield! consume from true (i + 1) 
    | Lower _ -> yield! consume from false (i + 1) 
    | _ -> yield! restart (i + 1) }
  // Consume are letters of the same kind (either all lower or all upper)
  and consume from takeUpper i = seq {
    match tryAt s i with
    | Lower _ when not takeUpper -> yield! consume from takeUpper (i + 1)
    | Upper _ when takeUpper -> yield! consume from takeUpper (i + 1)
    | _ -> 
        yield from, i
        yield! restart i }
    
  // Split string into segments and turn them to PascalCase
  seq { for i1, i2 in restart 0 do 
          let sub = s.Substring(i1, i2 - i1) 
          if Seq.forall Char.IsLetterOrDigit sub then
            yield sub.[0].ToString().ToUpper() + sub.ToLower().Substring(1) }
  |> String.concat ""

/// Turns a given non-empty string into a nice 'camelCase' identifier
let niceCamelName (s:string) = 
  let name = nicePascalName s
  if name.Length > 0 then
    name.[0].ToString().ToLowerInvariant() + name.Substring(1)
  else name

/// Given a function to format names (such as `niceCamelName` or `nicePascalName`)
/// returns a name generator that never returns duplicate name (by appending an
/// index to already used names)
/// 
/// This function is curried and should be used with partial function application:
///
///     let gen = nameGenerator nicePascalName
///     let n1 = gen "sample-name"
///     let n2 = gen "sample-name"
///
let uniqueGenerator niceName =
  let dict = new Dictionary<_, _>()
  (fun name ->
      let name = niceName name
      if dict.ContainsKey(name) then
        dict.[name] <- dict.[name] + 1
        sprintf "%s%d" name (dict.[name])
      else 
        dict.[name] <- 0
        name)

/// Trim HTML tags from a given string and replace all of them with spaces
/// Multiple tags are replaced with just a single space. (This is a recursive 
/// implementation that is somewhat faster than regular expression.)
let trimHtml (s:string) = 
  let chars = s.ToCharArray()
  let res = new Text.StringBuilder()

  // Loop and keep track of whether we're inside a tag or not
  let rec loop i emitSpace inside = 
    if i >= chars.Length then () else
    let c = chars.[i] 
    match inside, c with
    | true, '>' -> loop (i + 1) false false
    | false, '<' -> 
        if emitSpace then res.Append(' ') |> ignore
        loop (i + 1) false true
    | _ -> 
        if not inside then res.Append(c) |> ignore
        loop (i + 1) true inside

  loop 0 false false      
  res.ToString().TrimEnd()

/// Return a plural of an English word
let pluralize s =
  Pluralizer.toPlural s
