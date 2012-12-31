module ProviderImplementation.Providers

open System
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices

[<assembly:TypeProviderAssembly>]
do
    let handler = 
        ResolveEventHandler(fun _ args ->
        try 
            let assemName = AssemblyName(args.Name)

            if assemName.Name = "FSharp.Core" && assemName.Version.ToString() = "2.3.5.0" then 
                let dir = System.Environment.GetFolderPath(System.Environment.SpecialFolder.ProgramFilesX86)
                let (++) a b = System.IO.Path.Combine(a,b)
                Assembly.LoadFrom(dir ++ "Reference Assemblies" ++ "Microsoft" ++ "FSharp" ++ "3.0" ++ "Runtime" ++ ".NETPortable" ++ "FSharp.Core.dll")
            else 
                null
        with e ->  
            null)
    AppDomain.CurrentDomain.add_AssemblyResolve(handler)
