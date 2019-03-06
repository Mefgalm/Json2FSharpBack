[<RequireQualifiedAccess>]
module FsharpNewtonsoftHandler

open Types

let private fieldToView (field: Field) = 
    let jsonPropertyAttributes fieldName = sprintf "[<JsonProperty(\"%s\")>]" fieldName
    let fieldRow name fieldType = sprintf "%s: %s" name fieldType

    [
        jsonPropertyAttributes field.RawName
        fieldRow field.Name (field.Template.Replace("%s", field.Type))
    ]

let private typeToView (typeDef: Type) =
    let head = sprintf "type %s =" typeDef.Name
    let ident = "    "

    match typeDef.Fields with
    | [] -> sprintf "%s {}" head
    | fields -> 
        let fieldWithAttrs = 
            fields
            |> List.map fieldToView
            |> List.reduce (fun x y -> x @ [""] @ y)

        let fieldBlock =
            fieldWithAttrs
            |> List.mapi(fun i x -> 
                            match i with
                            | index when index = 0 || index = fieldWithAttrs.Length - 1 ->
                                let checkFirst = if index = 0 then sprintf "{ %s" x else x
                            
                                if index = fieldWithAttrs.Length - 1 then
                                    sprintf "  %s }" checkFirst
                                else checkFirst
                            | _ -> sprintf "  %s" x)
            |> List.map (sprintf "%s%s" ident)        
            |> List.reduce (sprintf "%s\n%s")

        sprintf "%s\n%s" head fieldBlock

let toView = 
    function 
    | Ok types -> Ok (types |> List.map typeToView |> List.reduce (sprintf "%s\n\n%s"))
    | Error msg -> Error msg    