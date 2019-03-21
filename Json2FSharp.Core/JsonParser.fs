﻿module JsonParserCore

open System
open FParsec
open Types
open System.Text.RegularExpressions

let ws = spaces 
let str s = pstring s

let stringLiteral =
    let escape = anyOf "\"\\/bfnrt"
                  |>> function
                      | 'b' -> "\b"
                      | 'f' -> "\u000C"
                      | 'n' -> "\n"
                      | 'r' -> "\r"
                      | 't' -> "\t"
                      | c   -> string c

    let unicodeEscape =
        str "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
            let hex2int c = (int c &&& 15) + (int c >>> 6)*9 
            (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
            |> char |> string
        )

    between (str "\"") (str "\"")
            (stringsSepBy (manySatisfy (fun c -> c <> '"' && c <> '\\'))
                          (str "\\" >>. (escape <|> unicodeEscape)))

let stringOrDateTime (str: string) =
    if DateTimeOffset.TryParse(str, ref (DateTimeOffset())) then JDateTimeOffset
    elif Regex.IsMatch(str, "^([0-9A-Fa-f]{8}[-][0-9A-Fa-f]{4}[-][0-9A-Fa-f]{4}[-][0-9A-Fa-f]{4}[-][0-9A-Fa-f]{12})$") then JGuid
    else JString

let jstringOrDateOrGuid = stringLiteral |>> stringOrDateTime

let jnumber =
    let options =
        NumberLiteralOptions.AllowExponent |||
        NumberLiteralOptions.AllowFraction |||
        NumberLiteralOptions.AllowMinusSign
    numberLiteral options "number" |>> (fun x -> if x.HasFraction then JFloat else JInt)

let jtrue  = stringReturn "true"  JBool
let jfalse = stringReturn "false" JBool
let jnull  = (stringReturn "null" JNull <|> stringReturn "undefined" JNull)

let jvalue, jvalueRef = createParserForwardedToRef() 

let listBetweenStrings sOpen sClose pElement f =
    between (str sOpen) (str sClose)
            (ws >>. sepBy (pElement .>> ws) (str "," .>> ws) |>> f)

let keyValue = tuple2 stringLiteral (ws >>. str ":" >>. ws >>. jvalue)

let jlist   = listBetweenStrings "[" "]" jvalue JList
let jobject = listBetweenStrings "{" "}" keyValue JObject

do jvalueRef := choice [jobject
                        jlist
                        jnumber
                        jstringOrDateOrGuid                        
                        jtrue
                        jnull
                        jfalse]

let json = ws >>. jobject .>> ws .>> eof

let parseJsonString str = run json str

let inline (^) f x = f x
let (<||>) f1 f2 x = f1 x || f2 x
let (<&&>) f1 f2 x = f1 x && f2 x

let isDateTimeOffset = function JDateTimeOffset _ | JDateTimeOffsetOption _ -> true | _ -> false
let isArray = function JArray _ | JArrayOption _ -> true | _ -> false
let isList = function JList _ -> true | _ -> false
let isGuid = function JGuid _ -> true | _ -> false
let isString = function JStringOption | JString -> true | _ -> false
let isNumber = function JInt | JFloat| JIntOption | JFloatOption -> true | _ -> false
let isNull = function JNull -> true | _ -> false
let isBool = function JBool | JBoolOption -> true | _ -> false
let isObject = function JObject _ | JObjectOption _ -> true | _ -> false

let isDateTimeOption = function JDateTimeOffsetOption -> true | _ -> false
let isBoolOption = function JBoolOption -> true | _ -> false
let isStringOption = function JStringOption -> true | _ -> false
let isArrayOption = function JArrayOption _ -> true | _ -> false
let isObjectOption = function JObjectOption _ -> true | _ -> false
let isNumberOption = function JIntOption | JFloatOption -> true | _ -> false
let isGuidOption = function JGuidOption -> true | _ -> false
let numberTypeOrder = function 
    | JInt | JIntOption -> 1 
    | JFloat | JFloatOption -> 2
    | _ -> failwith "Not a number type"

let checkStringOption = List.exists (isNull <||> isStringOption)
let checkGuidOption = List.exists (isNull <||> isGuidOption)
let checkArrayOption = List.exists (isNull <||> isArrayOption)
let checkObjectOption = List.exists (isNull <||> isObjectOption)
let checkNumberOption = List.exists (isNull <||> isNumberOption)
let checkBoolOption = List.exists (isNull <||> isBoolOption)
let checkDateTimeOption = List.exists (isNull <||> isDateTimeOption)

let (|EmptyList|_|) = function 
    | [] -> Some EmptyList
    | _ -> None

let (|NullList|_|) = function 
    | list when list |> List.forall isNull -> Some ^ NullList
    | _ -> None

let (|NumberList|_|) = function 
    | list when list |> List.forall (isNumber <||> isNull) -> Some ^ NumberList list
    | _ -> None

let (|StringList|_|) = function 
    | list when list |> List.forall (isString <||> isDateTimeOffset <||> isGuid <||> isNull) -> Some ^ StringList list
    | _ -> None

let (|DateTimeOffsetList|_|) = function 
    | list when list |> List.forall (isDateTimeOffset <||> isNull) -> Some ^ DateTimeOffsetList list
    | _ -> None

let (|BoolList|_|) = function 
    | list when list |> List.forall (isBool <||> isNull) -> Some ^ BoolList list
    | _ -> None

let (|ObjectList|_|) = function 
    | list when list |> List.forall (isObject <||> isNull) -> Some ^ ObjectList list
    | _ -> None

let (|ListList|_|) = function 
    | list when list |> List.forall (isList <||> isNull) -> Some ^ ListList list
    | _ -> None

let (|ArrayList|_|) = function 
    | list when list |> List.forall (isArray <||> isNull) -> Some ^ ArrayList list
    | _ -> None

let rec aggreagateListToSingleType jsonList =
    let getOptionType isOption istanceType =
        match istanceType, isOption with
        | JInt, true -> JIntOption
        | JFloat, true -> JFloatOption
        | JBool, true -> JBoolOption
        | JString, true -> JStringOption
        | JEmptyObject, true -> JEmptyObjectOption
        | JDateTimeOffset, true -> JDateTimeOffsetOption
        | JObject x, true -> JObjectOption x
        | JArray x, true -> JArrayOption x
        | JGuid, true -> JGuidOption
        | x, _ -> x

    match jsonList with
    | EmptyList -> JEmptyObject
    | NullList -> JEmptyObjectOption
    | StringList list -> 
        let getType = function
        | list when list |> List.forall (isDateTimeOffset <||> isNull) -> JDateTimeOffset
        | list when list |> List.forall (isGuid <||> isNull) -> JGuid
        | _ -> JString

        (getType list) |> getOptionType (list |> checkStringOption)
    | DateTimeOffsetList list -> JDateTimeOffset |> getOptionType (list |> checkDateTimeOption)
    | BoolList list -> JBool |> getOptionType (list |> checkBoolOption)
    | NumberList list ->
            list 
            |> List.filter (not << isNull)
            |> List.distinct
            |> List.map(fun x -> (x, numberTypeOrder x))
            |> List.maxBy snd
            |> fst
            |> getOptionType (list |> checkNumberOption)
    | ObjectList list ->
            let notNullList = list |> List.filter (not << isNull) 

            notNullList            
            |> List.map(function JObject list | JObjectOption list -> list)
            |> List.collect id
            |> List.groupBy fst
            |> List.map(fun (key, value) -> 
                if value.Length <> notNullList.Length then                    
                    (key, (getOptionType true (aggreagateListToSingleType (value |> List.map snd))))
                else (key, (aggreagateListToSingleType (value |> List.map snd))))
            |> JObject
            |> getOptionType (list |> checkObjectOption)
    | ListList list ->
            list 
            |> List.filter (not << isNull)
            |> List.map(function JList x -> x)
            |> List.collect id           
            |> aggreagateListToSingleType
            |> JArray
            |> getOptionType (list |> checkArrayOption)    
    | ArrayList list ->
            list 
            |> List.filter (not << isNull) 
            |> List.map(function JArray list | JArrayOption list -> list)
            |> aggreagateListToSingleType
            |> JArray
            |> getOptionType (list |> checkArrayOption) 
    | list when list |> List.exists(function 
                                    | JNull 
                                    | JIntOption 
                                    | JFloatOption 
                                    | JGuidOption
                                    | JEmptyObjectOption 
                                    | JBoolOption
                                    | JObjectOption _
                                    | JArrayOption _ 
                                    | JDateTimeOffsetOption -> true
                                    | _ -> false) -> JEmptyObjectOption
    | _ -> JEmptyObject

let rec private extractObject json =
    match json with
    | JArray obj 
    | JArrayOption obj -> extractObject obj
    | JObject _ 
    | JObjectOption _  -> Some json
    | _ -> None

let rec private fieldHandler fixName idGenerator collectionGenerator name json = 
    let getType { Name = _; Type = newType } = newType        
    let getTemplate { Name = _; Type = _; Template = template } = template    
    
    let fieldGenerate typeId name rawName typeName template =
        { TypeId = typeId; Name = name; RawName = rawName; Type = typeName; Template = template }

    match json with
    | JBool ->                  fieldGenerate None (name |> fixName) name "bool" "%s"
    | JBoolOption ->            fieldGenerate None (name |> fixName) name "bool" "%s option"
    | JNull ->                  fieldGenerate None (name |> fixName) name "Object" "%s option"
    | JInt ->                   fieldGenerate None (name |> fixName) name "int64" "%s"
    | JIntOption ->             fieldGenerate None (name |> fixName) name "int64" "%s option"
    | JGuid ->                  fieldGenerate None (name |> fixName) name "Guid" "%s"
    | JGuidOption ->            fieldGenerate None (name |> fixName) name "Guid" "%s option"
    | JFloat ->                 fieldGenerate None (name |> fixName) name "float" "%s"
    | JFloatOption ->           fieldGenerate None (name |> fixName) name "float" "%s option"
    | JString ->                fieldGenerate None (name |> fixName) name "string" "%s"
    | JDateTimeOffset ->        fieldGenerate None (name |> fixName) name "DateTimeOffset" "%s"
    | JDateTimeOffsetOption ->  fieldGenerate None (name |> fixName) name "DateTimeOffset" "%s option"
    | JStringOption ->          fieldGenerate None (name |> fixName) name "string" "%s option"
    | JEmptyObjectOption ->     fieldGenerate None (name |> fixName) name "Object" "%s option"
    | JEmptyObject ->           fieldGenerate None (name |> fixName) name "Object" "%s"
    | JObject _ ->              
        let fixedName = (name |> fixName)
        fieldGenerate (Some ^ idGenerator ()) fixedName name fixedName "%s"
    | JObjectOption _ ->        
        let fixedName = (name |> fixName)
        fieldGenerate (Some ^ idGenerator ()) fixedName name fixedName "%s option"
    | JArray obj -> 
        let next = fieldHandler fixName idGenerator collectionGenerator name obj

        fieldGenerate (Some ^ idGenerator ()) (name |> fixName) name (next |> getType) (next |> getTemplate |> collectionGenerator)
    | JArrayOption obj -> 
        let next = fieldHandler fixName idGenerator collectionGenerator name obj
        let optionTemplate = next |> getTemplate |> collectionGenerator |> sprintf "%s option"

        fieldGenerate (Some ^ idGenerator ()) (name |> fixName) name (next |> getType) optionTemplate
    | _ -> failwith "translateToString unexcpected"

let private typeHandler fixName (Some id) name fields = { Id = id; Name = name |> fixName; Fields = fields }

let compareType opt value = 
    match opt with
    | Some x -> x = value
    | None -> false

let private generateUniqueNames nameGenerator =
    List.mapi (fun i x -> i, x)
    >> List.groupBy(fun (_, x) -> x.Name)
    >> List.map(fun (_, group) ->
                match group with
                | [_] as list -> list 
                | list -> list |> List.mapi (fun i (order, nType) ->  order, { nType with Name = nameGenerator nType.Name i }))
    >> List.collect id
    >> List.sortBy fst
    >> List.map snd

let private renameAllField types =
    types |> List.map(fun x ->
                    { x with Fields = 
                                x.Fields 
                                |> List.map (fun field -> 
                                             match  types |> List.tryFind(fun q -> compareType field.TypeId q.Id) with
                                             | None -> field
                                             | Some value -> { field with Type = value.Name }) })

let private idGenerator () = Guid.NewGuid().ToString()
                                            
let private buildTypes rootObjectName fixName collectionGenerator json =  
    let build rootObjectName fixName collectionGenerator json =
        let rec tailDeep acc jobjs =
            match jobjs with
            | [] -> acc
            | (name, id, (JObject list))::xs 
            | (name, id, (JObjectOption list))::xs ->
                let newType = 
                    list
                    |> List.map (fun (key, value) ->
                        let row = 
                            match value with
                            | JList l -> JArray ^ aggreagateListToSingleType l
                            | _ -> value
                        fieldHandler fixName idGenerator collectionGenerator key row, row)
                    |> List.distinctBy (fun (field, _) -> field.Name)

                let newJobjs = 
                    newType 
                    |> List.map(fun (field, json) -> (field.Name, field.TypeId, extractObject json))
                    |> List.choose(fun (key, id, v) -> match v with Some j -> Some (key, id, j) | None -> None)
             
                tailDeep ((typeHandler fixName id name (newType |> List.map fst))::acc) (newJobjs @ xs)
            | _ -> failwith "unexpected"

        let types = tailDeep [] [rootObjectName |> fixName, Some ^ idGenerator(), json]

        types 
        |> generateUniqueNames (sprintf "%s%d")
        |> renameAllField
        |> JsonResult.Ok
        
    let rootName = if rootObjectName = "" then "Root" else rootObjectName

    build rootName fixName collectionGenerator json

let generateRecords fixName rootObjectName collectionGenerator (str: string) =
    match parseJsonString str with
    | Success(result, _, _) -> buildTypes rootObjectName fixName collectionGenerator result        
    | Failure(errorMsg, _, _) -> JsonResult.Error ^ errorMsg