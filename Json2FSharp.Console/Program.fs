open System
open System.Globalization
open System.Threading
open JsonBuilder
open Newtonsoft.Json
open System.Linq
open System.Text
open Microsoft.FSharp.Reflection

type ListGeneratorType =
    | List
    | Array
    | Sequence
    | CharpList

type TypeGeneration =
    | JustTypes
    | NewtosoftAttributes

[<CLIMutable>]
type GenerationParams =
    { Data: string
      RootObjectName : string
      ListGeneratorType: ListGeneratorType 
      TypeGeneration:  TypeGeneration }

[<EntryPoint>]
let main argv =
   let x: GenerationParams = 
      { Data = "string" 
        RootObjectName =  "root"
        ListGeneratorType= ListGeneratorType.List
        TypeGeneration= TypeGeneration.JustTypes};

   printfn "%A" (Newtonsoft.Json.JsonConvert.SerializeObject(x))
   0
