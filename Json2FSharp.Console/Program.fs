open System
open System.Globalization
open System.Threading
open JsonBuilder
open Newtonsoft.Json
open System.Linq
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
    "rating_value":"3.8",
    "date_published":"2013-­04-­23"
}"""  
    
    let dateFormats = [|"yyyy-MM-dd"; "dd/MM/yyyy"; "d/MM/yyyy"; "dd.MM.yyyy"; "yyyy-M-d"; "d.M.yyyy";
     "dd-MM-yyyy"; "MM/dd/yyyy"; "d.MM.yyyy"; "d/M/yyyy"; "MM-dd-yyyy"; "dd.MM.yyyy."; "yyyy.MM.dd.";
     "yyyy/MM/dd"; "yyyy. M. d"; "yyyy.M.d"; "yyyy.d.M"; "d.M.yyyy."; "d-M-yyyy"; "M/d/yyyy"; "yyyy/M/d"|]
    
    let timeFormats = [|"HH:mm"
                        "hh:mm tt"
                        
                        "HH:mm:ss"
                        "hh:mm:ss tt"
                        
                        "HH:mm:ss.f"
                        "hh:mm:ss.f tt"
                        
                        "HH:mm:ss.ff"
                        "hh:mm:ss.ff tt"
                        
                        "HH:mm:ss.fff"
                        "hh:mm:ss.fff tt"|]
    
    let mustHaveFormats =
       [|"yyyy-MM-ddTHH:mm:ss.fff'Z'"|]
    
    let dateTimeFormats =
       dateFormats
       |> Array.map (fun date ->
                     let dateAndTimeFormats = timeFormats |> Array.map (fun time -> sprintf "%s %s" date time)
                     Array.append [|date|] dateAndTimeFormats)
       |> Array.collect id
       |> Array.append mustHaveFormats
       
    let isDateTime (line: string) =
       DateTimeOffset.TryParseExact(line, dateTimeFormats, CultureInfo.InvariantCulture, DateTimeStyles.None)
       |> fst
       
    printfn "%A" (isDateTime "2012-04-23T18:25:43.511Z")       
    
    let result = (generateRecords FsharpCommon.fixName "Root" FsharpCommon.listGenerator input2) |> FsharpSimpleTypeHandler.toView
    
    
    0
