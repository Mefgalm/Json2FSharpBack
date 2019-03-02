// Learn more about F# at http://fsharp.org

open System
open JsonParserCore

[<EntryPoint>]
let main argv =
    let test = @"{
    ""glossary"": {
        ""title"": ""example glossary"",
    ""GlossDiv"": {
            ""title"": ""S"",
      ""GlossList"": {
                ""GlossEntry"": {
                    ""ID"": ""SGML"",
          ""SortAs"": ""SGML"",
          ""GlossTerm"": ""Standard Generalized Markup Language"",
          ""Acronym"": ""SGML"",
          ""Abbrev"": ""ISO 8879:1986"",
          ""GlossDef"": {
                        ""para"": ""A meta-markup language, used to create markup languages such as DocBook."",
            ""GlossSeeAlso"": [""GML"", ""XML""]
                    },
          ""GlossSee"": ""markup""
                }
            }
        }
    }
}"

    let result = (generateRecords FsharpCommon.fixName "Root" FsharpCommon.listGenerator test) |> FsharpNewtonsoftHandler.toView

    printfn "%A" result

    Console.ReadKey()
    0 // return an integer exit code
