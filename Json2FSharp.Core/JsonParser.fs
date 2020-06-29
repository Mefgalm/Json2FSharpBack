module Json2FSharp.Core.JsonParser

open System
open FParsec
open Types
open System.Text.RegularExpressions
open System.Globalization

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

let dateTimeFormats =
    let dateFormats =
         [|"yyyy-MM-dd"; "dd/MM/yyyy"; "d/MM/yyyy"; "dd.MM.yyyy"; "yyyy-M-d"; "d.M.yyyy";
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
        
    dateFormats
    |> Array.map (fun date ->
                  let dateAndTimeFormats = timeFormats |> Array.map (fun time -> sprintf "%s %s" date time)
                  Array.append [|date|] dateAndTimeFormats)
    |> Array.collect id
    |> Array.append mustHaveFormats       

let isDateTime (line: string) =
    DateTimeOffset.TryParseExact(line, dateTimeFormats, CultureInfo.InvariantCulture, DateTimeStyles.None)
    |> fst


let stringOrDateTime (str: string) =
    if isDateTime str then JDateTimeOffset
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
let jobjectList, jobjectListRef = createParserForwardedToRef() 

let listBetweenStrings sOpen sClose pElement f =
    between (str sOpen) (str sClose)
            (ws >>. sepBy (pElement .>> ws) (str "," .>> ws) |>> f)

let keyValue = tuple2 stringLiteral (ws >>. str ":" >>. ws >>. jvalue)

let jlist   = listBetweenStrings "[" "]" jvalue JList
let jobject = listBetweenStrings "{" "}" keyValue JObject

do jobjectListRef := listBetweenStrings "[" "]" (jobject <|> jobjectList <|> jnull) JList

do jvalueRef := choice [jobject
                        jlist
                        jnumber
                        jstringOrDateOrGuid                        
                        jtrue
                        jnull
                        jfalse]

let json = ws >>. (jobject <|> jobjectList) .>> ws .>> eof

let parseJsonString str = run json str