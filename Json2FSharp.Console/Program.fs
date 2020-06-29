open System
open System.Globalization
open System.Threading
open JsonBuilder
open Newtonsoft.Json
open System.Linq
open System.Text
open Microsoft.FSharp.Reflection

type A = {
   DT1: DateTime
   DT2: DateTimeOffset
   DT3: string
}

type OptionTypeConverter() =
  inherit JsonConverter()

  override x.CanConvert(t) =
    t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>

  override x.WriteJson(writer, value, serializer) =
    let value =
      if value = null then null
      else
        let _,fields = FSharpValue.GetUnionFields(value, value.GetType())
        fields.[0]
    serializer.Serialize(writer, value)

  override x.ReadJson(reader, t, existingValue, serializer) =
    let innerType = t.GetGenericArguments().[0]
    let innerType =
      if innerType.IsValueType then (typedefof<Nullable<_>>).MakeGenericType([|innerType|])
      else innerType
    let value = serializer.Deserialize(reader, innerType)
    let cases = FSharpType.GetUnionCases(t)
    if value = null then FSharpValue.MakeUnion(cases.[0], [||])
    else FSharpValue.MakeUnion(cases.[1], [|value|])

[<EntryPoint>]
let main argv =
    let input = """{
   "test":{
      "arr":[
         {
            "test":{
               "a":2,
               "b":{
                  "c":2
               }
            }
         },
         {
            "test":{
               "a":2,
               "b":{
                  "c":2
               }
            }
         }
      ]
   },
   "bbb": {
      "c": 2
   }
}"""
                
    let input2 = """{
  "top_by_one" : [
  {
     "topFoo" : "lol",
     "topTeam" : { "TeamName" : "team!" }
  }
  ],
  "top_by_two" : [
  {
    "topFoo" : "lol? Are you crazy?",
    "topTeam" : { "TeamName"  : "It's not a team!" }
  }
  ]
}"""  
    
    
    let result = (generateRecords FsharpCommon.fixName "Root" FsharpCommon.listGenerator input2) 
    
    printfn "%A" result
    
    0
