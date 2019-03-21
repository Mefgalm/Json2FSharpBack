# Json2FSharpBack

### About

This project provides generation F# records from Json

### Todo
1) Support array as root object
2) Support primitives types as root object
3) Refactor to be better open-source project
4) Option to construct instance of json object

### Sample
Json 
```
{
    "string": "text",
    "int": 1,
    "float": 1.0,
    "array": [],
    "stringArray": ["hello", "world"],
    "nestedObject": {
       "dateTime": "012-04-23T18:25:43.511Z",
       "guid": "f27a5b7f-0b7c-4e79-aff5-bdb0d34f3a9f",
       "null": null,
       "undefined": undefined
    },
   "complexArray": [
       {
           "int": 1,
           "string": "text"
       },
       {
           "int": null
       }
   ]
}
```
F# records
```
type ComplexArray =
    { Int: int64 option
      String: string option }

type NestedObject =
    { DateTime: DateTimeOffset
      Guid: Guid
      Null: Object option
      Undefined: Object option }

type Root =
    { String: string
      Int: int64
      Float: float
      Array: Object list
      StringArray: string list
      NestedObject: NestedObject
      ComplexArray: ComplexArray list }
```
