[<RequireQualifiedAccess>]
module FsharpCommon

open System
open System.Text.RegularExpressions

let form =  "string"

let listGenerator x = "%s list".Replace("%s", x)
let arrayGenerator x = "%s array".Replace("%s", x)
let charpListGenerator x = "List<%s>".Replace("%s", x)

let fixName (name: string) =
    let getFirstChar (name: string) = name.Chars 0
    let toUpperFirst (name: string) = (name |> getFirstChar |> Char.ToUpper).ToString() + name.[1..]
    let newFieldName = 
        Regex.Replace(name, "[^A-Za-z0-9\-_]", "").Split([|'-'; '_'|], StringSplitOptions.RemoveEmptyEntries) 
        |> Seq.ofArray 
        |> Seq.map(fun (x: string) -> if x.Length > 1 then x |> toUpperFirst else x)
        |> Seq.fold(+) ""

    if newFieldName.Length = 0 then failwith "Name should be not empty"

    if (not <| Char.IsLetter (getFirstChar name)) && getFirstChar name <> '_' then
        "The" + newFieldName |> toUpperFirst
    else
        newFieldName |> toUpperFirst