open System
open FParsec
open Newtonsoft.Json
open Microsoft.FSharp
open Microsoft.FSharp.Reflection
open JsonParser
open Types


type OptionConverter() =
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

let main argv =

    let testExample = @"{""menu"": {
    ""header"": ""SVG Viewer"",
    ""items"": [
        {""id"": ""Open""},
        {""id"": ""OpenNew"", ""label"": ""Open New""},
        null,
        {""id"": ""ZoomIn"", ""label"": ""Zoom In""},
        {""id"": ""ZoomOut"", ""label"": ""Zoom Out""},
        {""id"": ""OriginalView"", ""label"": ""Original View""},
        null,
        {""id"": ""Quality""},
        {""id"": ""Pause""},
        {""id"": ""Mute""},
        null,
        {""id"": ""Find"", ""label"": ""Find...""},
        {""id"": ""FindAgain"", ""label"": ""Find Again""},
        {""id"": ""Copy""},
        {""id"": ""CopyAgain"", ""label"": ""Copy Again""},
        {""id"": ""CopySVG"", ""label"": ""Copy SVG""},
        {""id"": ""ViewSVG"", ""label"": ""View SVG""},
        {""id"": ""ViewSource"", ""label"": ""View Source""},
        {""id"": ""SaveAs"", ""label"": ""Save As""},
        null,
        {""id"": ""Help""},
        {""id"": ""About"", ""label"": ""About Adobe CVG Viewer...""}
    ]
}}"

    let output = (generateRecords FsharpCommon.fixName "Root" FsharpCommon.listGenerator testExample) |> FsharpSimpleTypeHandler.toView

    printfn "%s" output

    Console.ReadKey() |> ignore

    0