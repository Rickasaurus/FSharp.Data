// --------------------------------------------------------------------------------------
// Specifies the DesignTime implementation of the F# type providers
// --------------------------------------------------------------------------------------

namespace global
open Microsoft.FSharp.Core.CompilerServices

[<assembly:TypeProviderAssembly("FSharp.Data.DesignTime")>]
do()

