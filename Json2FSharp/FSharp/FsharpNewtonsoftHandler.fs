[<RequireQualifiedAccess>]
module FsharpNewtonsoftHandler

open Types

let private fixName = FsharpCommon.fixName

let private fieldToView (field: Field) = 
    let jsonPropertyAttributes fieldName = sprintf "[<JsonProperty(%s)>]" fieldName

    sprintf "%s\n%s: %s" (jsonPropertyAttributes field.Name) (field.Name |> fixName) field.Type

let private typeToView (typeDef: Type) =    
    let head = sprintf "type %s =" (typeDef.Name |> fixName)

    match typeDef.Fields with
    | [] -> sprintf "%s {}" head
    | fields -> 
        let fieldWithAttrs = 
            fields
            |> List.map fieldToView      

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
            |> List.map (sprintf "\t%s")        
            |> List.reduce (sprintf "%s\n%s")

        sprintf "%s\n%s" head fieldBlock

let toView = 
    function 
    | Ok types -> Ok (types |> List.map typeToView |> List.reduce (sprintf "%s\n\n%s"))
    | Error msg -> Error msg
    


