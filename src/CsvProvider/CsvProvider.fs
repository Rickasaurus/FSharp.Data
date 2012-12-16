// --------------------------------------------------------------------------------------
// Copyright (c) Microsoft Corporation 2005-2011.
// This sample code is provided "as is" without warranty of any kind. 
// We disclaim all warranties, either express or implied, including the 
// warranties of merchantability and fitness for a particular purpose. 
// --------------------------------------------------------------------------------------

namespace ProviderImplementation

open System
open System.IO
open System.Reflection
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.StructureInference
open FSharp.Data.Csv

// --------------------------------------------------------------------------------------
// Inference
// --------------------------------------------------------------------------------------

module CsvInference = 
  /// Infers the type of a CSV file using the specified number of rows
  /// (This handles units in the same way as the original MiniCSV provider)
  let inferType (csv:CsvFile) count =

    // Infer the units and names from the headers
    let headers = csv.Headers |> Seq.map (fun header ->
      let m = Regex.Match(header, @"(?<field>.+) \((?<unit>.+)\)")
      if m.Success then
        let headerName = m.Groups.["field"].Value
        let unitName = m.Groups.["unit"].Value
        Some(ProvidedMeasureBuilder.Default.SI unitName), headerName
      else None, header)

    // Infer the type of collection using structural inference
    inferCollectionType
      [ for row in Seq.takeMax count csv.Data ->
          let fields = 
            [ for (unit, header), value in Seq.zip headers row.Columns ->
                let typ = inferPrimitiveType value unit
                { Name = header; Optional = false; Type = typ } ]
          Record(None, fields) ]

// --------------------------------------------------------------------------------------
//
// --------------------------------------------------------------------------------------

module private Quotations =

    open System.Collections.Generic
    open Microsoft.FSharp.Quotations.ExprShape
    open Microsoft.FSharp.Quotations.Patterns

    let replaceType (fromAsm, toAsm : Assembly) (t : Type) =
        if t.Assembly = fromAsm then
            toAsm.GetType t.FullName
        else
            t

    let replaceMember (fromAsm, toAsm : Assembly) (m : 'a when 'a :> MemberInfo) =
        if m.DeclaringType.Assembly = fromAsm then
            let t = toAsm.GetType m.DeclaringType.FullName
            t.GetMember(m.Name, m.MemberType, BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic) |> Seq.exactlyOne :?> 'a
        else
            m

    let replaceVar (fromAsm, toAsm : Assembly) (varTable: IDictionary<_,_>) reversePass (v: Var) =
        if reversePass then
            // store the mappings as we'll have to revert them later
            if v.Type.Assembly = fromAsm then
                assert (reversePass)
                let newVar = Var (v.Name, toAsm.GetType v.Type.FullName, v.IsMutable)
                varTable.Add(newVar, v)
                newVar
            else
                v
        else
            if v.Type.Assembly = fromAsm then
                varTable.[v]
            else
                v

    let rec replaceExpr replacement (varTable: IDictionary<_,_>) reversePass quotation =
        let re = replaceExpr replacement varTable reversePass
        let rt = replaceType replacement
        let inline rm m = replaceMember replacement m
        let rv = replaceVar replacement varTable reversePass

        match quotation with
        | Call (expr, m, exprs) -> 
            match expr with
            | Some expr -> Expr.Call (re expr, rm m, List.map re exprs)
            | None -> Expr.Call (rm m, List.map re exprs)
        | PropertyGet (expr, p, exprs) -> 
            match expr with
            | Some expr -> Expr.PropertyGet (re expr, rm p, List.map re exprs)
            | None -> Expr.PropertyGet (rm p, List.map re exprs)
        | ShapeVar v -> 
            Expr.Var (rv v)
        | ShapeLambda (v, expr) -> 
            Expr.Lambda (rv v, re expr)
        | ShapeCombination (o, exprs) -> 
            RebuildShapeCombination (o, List.map re exprs)

// --------------------------------------------------------------------------------------

open System.Collections.Generic

[<TypeProvider>]
type public CsvProvider(cfg:TypeProviderConfig) as this =
  inherit TypeProviderForNamespaces()

  // Generate namespace and type 'FSharp.Data.JsonProvider'
  let asm = Assembly.LoadFrom cfg.RuntimeAssembly

  let ns = "FSharp.Data"
  let xmlProvTy = ProvidedTypeDefinition(asm, ns, "CsvProvider", Some typeof<obj>)

  let variablesTable = new Dictionary<Var,Var>()
  let fixType = Quotations.replaceType (Assembly.GetExecutingAssembly(), asm)
  let fixTypes = Quotations.replaceExpr (Assembly.GetExecutingAssembly(), asm) variablesTable false
  let fixTypesReverse = Quotations.replaceExpr (asm, Assembly.GetExecutingAssembly()) variablesTable true

  let buildTypes (typeName:string) (args:obj[]) =

    // Generate the required type with empty constructor
    let resTy = ProvidedTypeDefinition(asm, ns, typeName, Some (fixType typeof<CsvFile>))

    // A type that is used to hide all generated domain types
    let domainTy = ProvidedTypeDefinition("DomainTypes", Some typeof<obj>)
    resTy.AddMember(domainTy)

    // Infer the schema from a specified file or URI sample
    let sample = 
      try CsvFile.Parse(ProviderHelpers.readFileInProvider cfg (args.[0] :?> string) )
      with _ -> failwith "Specified argument is not a well-formed CSV file."
    let infered = CsvInference.inferType sample Int32.MaxValue

    let (|Singleton|) = function Singleton s -> fixTypesReverse s

    let generateCsvType (domainType:ProvidedTypeDefinition) = function
        | Collection(SingletonMap(_, (_, Record(_, fields)))) ->
            let objectTy = ProvidedTypeDefinition("Row", Some (fixType typeof<CsvRow>))
            domainType.AddMember(objectTy)

            for index, field in fields |> Seq.mapi (fun i v -> i, v) do
              let baseTyp, propTyp =
                match field.Type with
                | Primitive(typ, Some unit) -> 
                    typ, ProvidedMeasureBuilder.Default.AnnotateType(typ, [unit])
                | Primitive(typ, None) -> typ, typ
                | _ -> typeof<string>, typeof<string>

              let p = ProvidedProperty(NameUtils.nicePascalName field.Name, propTyp)
              let _, conv = Conversions.convertValue field.Name false baseTyp fixType
              let conv = conv >> fixTypes
              p.GetterCode <- fun (Singleton row) -> conv <@@ Some((%%row:CsvRow).Columns.[index]) @@> 
              objectTy.AddMember(p)

            objectTy
        | _ -> failwith "generateCsvType: Type inference returned wrong type"


    let ctx = domainTy
    let methResTy = generateCsvType ctx infered

    // 'Data' property has the generated type
    let p = ProvidedProperty("Data", methResTy.MakeArrayType())
    p.GetterCode <- fun (Singleton self) -> fixTypes <@@ (%%self : CsvFile).Data @@>
    resTy.AddMember(p)
    
    // Generate static Parse method
    let args = [ ProvidedParameter("source", typeof<string>) ]
    let m = ProvidedMethod("Parse", args, resTy)
    m.IsStaticMethod <- true
    m.InvokeCode <- fun (Singleton source) -> fixTypes <@@ CsvFile.Parse(%%source) @@>
    resTy.AddMember(m)

    // Generate static Load method
    let args =  [ ProvidedParameter("path", typeof<string>) ]
    let m = ProvidedMethod("Load", args, resTy)
    m.IsStaticMethod <- true
    m.InvokeCode <- fun (Singleton source) -> fixTypes <@@ CsvFile.Parse(File.ReadAllText(%%source)) @@>
    resTy.AddMember(m)

    // Return the generated type
    resTy

  // Add static parameter that specifies the API we want to get (compile-time) 
  let parameters = [ ProvidedStaticParameter("Sample", typeof<string>) ]
  do xmlProvTy.DefineStaticParameters(parameters, buildTypes)

  // Register the main type with F# compiler
  do this.AddNamespace(ns, [ xmlProvTy ])