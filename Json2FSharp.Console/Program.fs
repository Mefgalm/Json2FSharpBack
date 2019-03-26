open System
open JsonParserCore
open Newtonsoft.Json
open Microsoft.FSharp.Reflection

type Items =
    { Id: string
      Label: string option }

type Menu =
    { Header: string
      Items: Items option list }

type Root = { Menu: Menu }

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
    let test = @"[
  {
  ""name"" : ""John"",
  ""last_Name"": null,
  ""middle_name"": """"
  },
  null,
  {
  ""name"" : ""John"",
  ""last_Name"": """",
  ""middle_name"": """"}
  ]"    

    //let testObj = Newtonsoft.Json.JsonConvert.DeserializeObject<Root>(test, [|OptionTypeConverter() :> JsonConverter|])

    let result = (generateRecords FsharpCommon.fixName "Root" FsharpCommon.listGenerator test) |> FsharpSimpleTypeHandler.toView

    printfn "%A" result

    Console.ReadKey() |> ignore
    0
