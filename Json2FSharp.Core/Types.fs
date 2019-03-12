module Types

type Field =
    { TypeId: string option
      Name: string
      Type: string
      RawName: string
      Template: string }

type Type =
    { Id: string
      Name: string
      Fields: Field list }

type JsonResult<'a> =
    | Ok of 'a
    | Error of string

type Json = 
    | JBool
    | JBoolOption
    | JGuid
    | JGuidOption
    | JNull
    | JInt
    | JIntOption
    | JFloat
    | JFloatOption
    | JString 
    | JDateTimeOffset
    | JDateTimeOffsetOption
    | JStringOption
    | JList of Json list
    | JEmptyObject
    | JEmptyObjectOption
    | JObject of (string * Json) list
    | JObjectOption of (string * Json) list
    | JArray of Json
    | JArrayOption of Json