module Tests

open System
open Xunit
open JsonParserCore
open Types

let pass () = Assert.True(true)
let fail () = Assert.True(false)

[<Fact>]
let ``Should be error if empty json`` () =
    let input = ""

    let result = generateRecords FsharpCommon.fixName "Root" FsharpCommon.listGenerator input

    match result with
    | Ok _ -> fail ()
    | Error _ -> pass ()    

[<Fact>]
let ``Should be empty object if only braces`` () =
    let root = "Root"
    let input = "{}"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input

    match result with    
    | Ok [{Name = name; Fields = [] }] when name = root -> pass ()
    | _ -> fail ()

[<Fact>]
let ``Should fail if root object name is empty`` () =
    let input = "{}"

    let result = generateRecords FsharpCommon.fixName "" FsharpCommon.listGenerator input

    match result with
    | Ok [{Name = "Root"; Fields = [] }] -> pass ()
    | _ -> fail ()

[<Fact>]
let ``Should fix if field is empty name`` () =
    let input = @"{ """": 3 }"

    let result = generateRecords FsharpCommon.fixName "Root" FsharpCommon.listGenerator input

    match result with
    | Ok [{Name = "Root"; Fields = [ { Name = "Empty"; Type = "int64"; Template = "%s" } ]}] -> pass ()
    | _ -> fail ()

[<Fact>]
let ``Should fix if field is contains only wrong characters`` () =
    let input = @"{ ""$%^&"": 3 }"

    let result = generateRecords FsharpCommon.fixName "Root" FsharpCommon.listGenerator input

    match result with
    | Ok [{Name = "Root"; Fields = [ { Name = "Empty"; Type = "int64"; Template = "%s" } ]}] -> pass ()
    | _ -> fail ()

[<Fact>]
let ``Should treat udentified as null`` () =
    let input = @"{ ""val"": undefined }"

    let result = generateRecords FsharpCommon.fixName "Root" FsharpCommon.listGenerator input

    match result with
    | Ok [{Name = "Root"; Fields = [ { Name = "Val"; Type = "Object"; Template = "%s option" } ]}] -> pass ()
    | _ -> fail ()

[<Fact>]
let ``Should treat udentified as null in int array`` () =
    let input = @"{ ""Arr"": [2, undefined] }"

    let result = generateRecords FsharpCommon.fixName "Root" FsharpCommon.listGenerator input

    match result with
    | Ok [{Name = "Root"; Fields = [ { Name = "Arr"; Type = "int64"; Template = "%s option list" } ]}] -> pass ()
    | _ -> fail ()

[<Fact>]
let ``Should treat udentified as null in object array`` () =
    let input = @"{ ""Arr"": [{""val"": 3}, undefined] }"

    let result = generateRecords FsharpCommon.fixName "Root" FsharpCommon.listGenerator input

    match result with
    | Ok [{Name = "Arr"; Fields = [ { Name = "Val"; Type = "int64"; Template = "%s"} ]}
          {Name = "Root"; Fields = [ { Name = "Arr"; Type = "Arr"; Template = "%s option list" } ]}] -> pass ()
    | _ -> fail ()

[<Fact>]
let ``Should identify guid`` () =
    let input = @"{ ""val"": ""f27a5b7f-0b7c-4e79-aff5-bdb0d34f3a9f"" }"

    let result = generateRecords FsharpCommon.fixName "Root" FsharpCommon.listGenerator input

    match result with
    | Ok [{Name = "Root"; Fields = [ { Name = "Val"; Type = "Guid"; Template = "%s" } ]}] -> pass ()
    | _ -> fail ()

[<Fact>]
let ``Should identify guid in array`` () =
    let input = @"{ ""arr"": [""f27a5b7f-0b7c-4e79-aff5-bdb0d34f3a9f""] }"

    let result = generateRecords FsharpCommon.fixName "Root" FsharpCommon.listGenerator input

    match result with
    | Ok [{Name = "Root"; Fields = [ { Name = "Arr"; Type = "Guid"; Template = "%s list" } ]}] -> pass ()
    | _ -> fail ()

[<Fact>]
let ``Should identify guid and string as string list`` () =
    let input = @"{ ""arr"": [""f27a5b7f-0b7c-4e79-aff5-bdb0d34f3a9f"", ""some string""] }"

    let result = generateRecords FsharpCommon.fixName "Root" FsharpCommon.listGenerator input

    match result with
    | Ok [{Name = "Root"; Fields = [ { Name = "Arr"; Type = "string"; Template = "%s list" } ]}] -> pass ()
    | _ -> fail ()

[<Fact>]
let ``Should identify dateTime and string as string list`` () =
    let input = @"{ ""arr"": [""2012-04-23T18:25:43.511Z"", ""some string""] }"

    let result = generateRecords FsharpCommon.fixName "Root" FsharpCommon.listGenerator input

    match result with
    | Ok [{Name = "Root"; Fields = [ { Name = "Arr"; Type = "string"; Template = "%s list" } ]}] -> pass ()
    | _ -> fail ()

[<Fact>]
let ``Should identify dateTime and guid as string list`` () =
    let input = @"{ ""arr"": [""2012-04-23T18:25:43.511Z"", ""f27a5b7f-0b7c-4e79-aff5-bdb0d34f3a9f""] }"

    let result = generateRecords FsharpCommon.fixName "Root" FsharpCommon.listGenerator input

    match result with
    | Ok [{Name = "Root"; Fields = [ { Name = "Arr"; Type = "string"; Template = "%s list" } ]}] -> pass ()
    | _ -> fail ()

[<Fact>]
let ``Should identify guid with null in array`` () =
    let input = @"{ ""arr"": [""f27a5b7f-0b7c-4e79-aff5-bdb0d34f3a9f"", null] }"

    let result = generateRecords FsharpCommon.fixName "Root" FsharpCommon.listGenerator input

    match result with
    | Ok [{Name = "Root"; Fields = [ { Name = "Arr"; Type = "Guid"; Template = "%s option list" } ]}] -> pass ()
    | _ -> fail ()

[<Fact>]
let ``Should and The in begin of rootObject`` () =
    let input = "{}"

    let result = generateRecords FsharpCommon.fixName "3" FsharpCommon.listGenerator input

    match result with
    | Ok [{Name = name; Fields = []}] when name = "The3" -> pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse string`` () =
    let root = "Root"
    let input = @"{ ""name"": ""test"" }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input

    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Name"; Type = "string"; Template = "%s" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Theory()>]
[<InlineData("4")>]
[<InlineData("-4")>]
[<InlineData("4e2")>]
[<InlineData("-4e2")>]
[<InlineData("4E-2")>]
[<InlineData("4e+2")>]
let ``Should parse int64`` (value: string) =
    let root = "Root"
    let input = sprintf @"{ ""age"": %s }" value

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input

    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Age"; Type = "int64"; Template = "%s" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should fail on plus sign in numbers per specification`` () =
    let root = "Root"
    let input = @"{ ""age"": +4 }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input

    match result with
    | Ok _ ->    fail ()
    | Error _ -> pass ()

[<Theory()>]
[<InlineData("3.")>]
[<InlineData("3.4")>]
[<InlineData("3.0")>]
[<InlineData("-3.0")>]
[<InlineData("3.e3")>]
[<InlineData("3.02e+3")>]
[<InlineData("3.02E-3")>]
[<InlineData("3.02e3")>]
let ``Should parse float`` (value: string) =
    let root = "Root"
    let input = sprintf @"{ ""age"": %s }" value

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input

    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Age"; Type = "float"; Template = "%s" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse object`` () =
    let root = "Root"
    let input = @"{ ""someObj"": {} }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input

    match result with
    | Ok [ { Name = "SomeObj"; Fields = [] }
           { Name = name; Fields = [ { TypeId = _; Name = "SomeObj"; Type = "SomeObj"; Template = "%s" }] }
         ] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse null`` () =
    let root = "Root"
    let input = @"{ ""emptyObj"": null }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input

    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "EmptyObj"; Type = "Object"; Template = "%s option" }] } ] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse DateTimeOffset`` () =
    let root = "Root"
    let input = @"{ ""date"": ""2012-04-23T18:25:43.511Z"" }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Date"; Type = "DateTimeOffset"; Template = "%s" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse bool`` () =
    let root = "Root"
    let input = @"{ ""isOk"": true }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "IsOk"; Type = "bool"; Template = "%s" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array`` () =
    let root = "Root"
    let input = @"{ ""arr"": [] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "Object"; Template = "%s list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array with null`` () =
    let root = "Root"
    let input = @"{ ""arr"": [null] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "Object"; Template = "%s option list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array with int64`` () =
    let root = "Root"
    let input = @"{ ""arr"": [1, 2] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "int64"; Template = "%s list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()


[<Fact>]
let ``Should parse array with float`` () =
    let root = "Root"
    let input = @"{ ""arr"": [1.3, 2.2] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "float"; Template = "%s list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array with string`` () =
    let root = "Root"
    let input = @"{ ""arr"": [""a"", ""b""] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "string"; Template = "%s list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array with bool`` () =
    let root = "Root"
    let input = @"{ ""arr"": [true, false] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "bool"; Template = "%s list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array with dateTimes`` () =
    let root = "Root"
    let input = @"{ ""arr"": [""2012-04-23T18:25:43.511Z"", ""2012-04-23T16:25:43.511Z""] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "DateTimeOffset"; Template = "%s list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array with objects`` () =
    let root = "Root"
    let input = @"{ ""arr"": [{}, {}] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [ { Name = "Arr"; Fields = []; }
           { Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "Arr"; Template = "%s list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array with arrays`` () =
    let root = "Root"
    let input = @"{ ""arr"": [[], []] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "Object"; Template = "%s list list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()


[<Fact>]
let ``Should parse array with float and int64s`` () =
    let root = "Root"
    let input = @"{ ""arr"": [3, 2.3] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "float"; Template = "%s list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array with int64s and null`` () =
    let root = "Root"
    let input = @"{ ""arr"": [3, null] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "int64"; Template = "%s option list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array with floats and null`` () =
    let root = "Root"
    let input = @"{ ""arr"": [3.2, null] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "float"; Template = "%s option list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array with int64s and floats and null`` () =
    let root = "Root"
    let input = @"{ ""arr"": [1, 3.2, null] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "float"; Template = "%s option list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array with strings and null`` () =
    let root = "Root"
    let input = @"{ ""arr"": [""Test"", null] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "string"; Template = "%s option list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array with dateTimes and null`` () =
    let root = "Root"
    let input = @"{ ""arr"": [""2012-04-23T18:25:43.511Z"", null] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "DateTimeOffset"; Template = "%s option list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array with bools and null`` () =
    let root = "Root"
    let input = @"{ ""arr"": [true, null] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "bool"; Template = "%s option list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array with objects and null`` () =
    let root = "Root"
    let input = @"{ ""arr"": [{}, null] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [ { Name = "Arr"; Fields = [] }
           { Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "Arr"; Template = "%s option list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array with array and null`` () =
    let root = "Root"
    let input = @"{ ""arr"": [[], null] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "Object"; Template = "%s list option list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array with int64s and strings`` () =
    let root = "Root"
    let input = @"{ ""arr"": [""test"", 43] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "Object"; Template = "%s list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()


[<Theory>]
[<InlineData("\"test\"", "3")>]
[<InlineData("\"test\"", "3.4")>]
[<InlineData("\"test\"", "true")>]
[<InlineData("\"test\"", "{}")>]
[<InlineData("\"test\"", "[]")>]
[<InlineData("2", "\"012-04-23T18:25:43.511Z\"")>]
[<InlineData("2", "true")>]
[<InlineData("2", "{}")>]
[<InlineData("2", "[]")>]
[<InlineData("2.1", "\"012-04-23T18:25:43.511Z\"")>]
[<InlineData("2.1", "true")>]
[<InlineData("2.1", "{}")>]
[<InlineData("2.1", "[]")>]
[<InlineData("true", "\"012-04-23T18:25:43.511Z\"")>]
[<InlineData("true", "{}")>]
[<InlineData("true", "[]")>]
[<InlineData("\"012-04-23T18:25:43.511Z\"", "{}")>]
[<InlineData("\"012-04-23T18:25:43.511Z\"", "[]")>]
[<InlineData("\"f27a5b7f-0b7c-4e79-aff5-bdb0d34f3a9f\"", "3")>]
[<InlineData("\"f27a5b7f-0b7c-4e79-aff5-bdb0d34f3a9f\"", "3.4")>]
[<InlineData("\"f27a5b7f-0b7c-4e79-aff5-bdb0d34f3a9f\"", "true")>]
[<InlineData("\"f27a5b7f-0b7c-4e79-aff5-bdb0d34f3a9f\"", "{}")>]
[<InlineData("\"f27a5b7f-0b7c-4e79-aff5-bdb0d34f3a9f\"", "[]")>]
let ``Should parse array with types that cant be matched as object`` firstValue secondValue =
    let root = "Root"
    let input = sprintf @"{ ""arr"": [%s, %s] }" firstValue secondValue

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "Object"; Template = "%s list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Theory()>]
[<InlineData("\"test\"", "3")>]
[<InlineData("\"test\"", "3.4")>]
[<InlineData("\"test\"", "true")>]
[<InlineData("\"test\"", "{}")>]
[<InlineData("\"test\"", "[]")>]
[<InlineData("2", "\"2012-04-23T18:25:43.511Z\"")>]
[<InlineData("2", "true")>]
[<InlineData("2", "{}")>]
[<InlineData("2", "[]")>]
[<InlineData("2.1", "\"2012-04-23T18:25:43.511Z\"")>]
[<InlineData("2.1", "true")>]
[<InlineData("2.1", "{}")>]
[<InlineData("2.1", "[]")>]
[<InlineData("true", "\"012-04-23T18:25:43.511Z\"")>]
[<InlineData("true", "{}")>]
[<InlineData("true", "[]")>]
[<InlineData("\"012-04-23T18:25:43.511Z\"", "{}")>]
[<InlineData("\"012-04-23T18:25:43.511Z\"", "[]")>]
let ``Should parse array with different types (object) and null as Object option`` firstValue secondValue =
    let root = "Root"
    let input = sprintf @"{ ""arr"": [%s, %s, null] }" firstValue secondValue

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "Object"; Template = "%s option list" }] }] when name = root ->         
        pass ()
    | _ -> fail ()

[<Theory()>]
[<InlineData("\"test\"", "string")>]
[<InlineData("2", "int64")>]
[<InlineData("2.1", "float")>]
[<InlineData("true", "bool")>]
[<InlineData("\"012-04-23T18:25:43.511Z\"", "DateTimeOffset")>]
let ``Should parse array and merge two objects with same type field`` value resType =
    let root = "Root"
    let input = sprintf @"{ ""arr"": [{ ""age"": %s }, { ""age"": %s }] }" value value

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [ { Name = arrName; Fields = [ { TypeId = _; Name = "Age"; Type = fieldType; Template = "%s" } ]}
           { Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "Arr"; Template = "%s list" }] }          
         ] when name = root && arrName = "Arr" && fieldType = resType ->         
        pass ()
    | _ -> fail ()

[<Theory()>]
[<InlineData("\"test\"", "string")>]
[<InlineData("2", "int64")>]
[<InlineData("2.1", "float")>]
[<InlineData("true", "bool")>]
[<InlineData("\"012-04-23T18:25:43.511Z\"", "DateTimeOffset")>]
let ``Should parse array and merge two objects with same type field with null`` value resType =
    let root = "Root"
    let input = sprintf @"{ ""arr"": [{ ""age"": %s }, { ""age"": %s }, {""age"": null }] }" value value

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [ { Name = "Arr"; Fields = [ { TypeId = _; Name = "Age"; Type = fieldType; Template = "%s option" } ]}
           { Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "Arr"; Template = "%s list" }] }
         ] when name = root && fieldType = resType ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array and merge two objects with different fields`` () =
    let root = "Root"
    let input = @"{ ""arr"": [{ ""age"": 4 }, { ""name"": ""Name"" }] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [ { Name = "Arr"; Fields = [ { TypeId = _; Name = "Age"; Type = "int64"; Template = "%s option" };
                                       { TypeId = _; Name = "Name"; Type = "string"; Template = "%s option" }
                                     ]}
           { Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "Arr"; Template = "%s list" }] }
         ] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array and merge two lists`` () =
    let root = "Root"
    let input = @"{ ""arr"": [[{ ""age"": 4 }], [{ ""name"": ""Name"" }]] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [ { Name = "Arr"; Fields = [ { TypeId = _; Name = "Age"; Type = "int64"; Template = "%s option" };
                                       { TypeId = _; Name = "Name"; Type = "string"; Template = "%s option" }
                                     ]}
           { Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "Arr"; Template = "%s list list" }] }
         ] when name = root ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array and ignore nulls`` () =
    let root = "Root"
    let input = @"{ ""arr"": [{ ""age"": 2 }, null] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [ { Name = arrName; Fields = [ { TypeId = _; Name = "Age"; Type = "int64"; Template = "%s" } ]}
           { Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "Arr"; Template = "%s option list" }] }
         ] when name = root && arrName = "Arr" ->         
        pass ()
    | _ -> fail ()

[<Fact>]
let ``Should parse array and merge two objects with int64s and floats`` () =
    let root = "Root"
    let input = @"{ ""arr"": [{ ""age"": 2 }, { ""age"": 4.2 }] }"

    let result = generateRecords FsharpCommon.fixName root FsharpCommon.listGenerator input
    
    match result with
    | Ok [ { Name = arrName; Fields = [ { TypeId = _; Name = "Age"; Type = "float"; Template = "%s" } ]}
           { Name = name; Fields = [ { TypeId = _; Name = "Arr"; Type = "Arr"; Template = "%s list" }] }
         ] when name = root && arrName = "Arr" ->         
        pass ()
    | _ -> fail ()


[<Fact>]
let ``Should remove incorrect symbols`` () =
    let rootName = "root!@#$^&*()+=\\.,~`¹;%:?*)! "
    let input = @"{}"

    let result = generateRecords FsharpCommon.fixName rootName FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = "Root"; Fields = [] }] -> pass ()
    | _ -> fail ()

[<Fact>]
let ``Should left numbers`` () =
    let input = @"{}"

    let result = generateRecords FsharpCommon.fixName "root1" FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = "Root1"; Fields = [] }] -> pass ()
    | _ -> fail ()

[<Fact>]
let ``Should fix underscore and minus`` () =
    let input = @"{}"

    let result = generateRecords FsharpCommon.fixName "root-test" FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = "RootTest"; Fields = [] }] -> pass ()
    | _ -> fail ()

[<Fact>]
let ``Should fix rootName if empty`` () =
    let input = @"{}"

    let result = generateRecords FsharpCommon.fixName "" FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = "Root"; Fields = [] }] -> pass ()
    | _ -> fail ()

[<Fact>]
let ``Should fix add The before`` () =
    let input = @"{}"

    let result = generateRecords FsharpCommon.fixName "1root" FsharpCommon.listGenerator input
    
    match result with
    | Ok [{ Name = "The1root"; Fields = [] }] -> pass ()
    | _ -> fail ()

[<Fact>]
let ``Should rename objects if with same names`` () =
    let input = @"{ ""obj"": { ""field"": 2 }, ""next"": { ""obj"": { ""age"": 2 } } }"

    let result = generateRecords FsharpCommon.fixName "Root" FsharpCommon.listGenerator input
    
    match result with
    | Ok [ { Name = "Obj0"; Fields = [ { TypeId = _; Name = "Age"; Type = "int64"; Template = "%s" }] }
           { Name = "Next"; Fields = [ { TypeId = _; Name = "Obj"; Type = "Obj0"; Template = "%s" } ]}
           { Name = "Obj1"; Fields = [ { TypeId = _; Name = "Field"; Type = "int64"; Template = "%s" } ]}
           { Name = "Root"; Fields = [ { TypeId = _; Name = "Obj"; Type = "Obj1"; Template = "%s" }
                                       { TypeId = _; Name = "Next"; Type = "Next"; Template = "%s" }
                                     ] }
         ] -> pass ()
    | _ -> fail ()

[<Fact>]
let ``Should support array as root object`` () =
    let input = "[]"

    let result = generateRecords FsharpCommon.fixName "Root" FsharpCommon.listGenerator input
    
    match result with
    | Ok [ { Name = "Root"; Fields = [] } ] -> pass ()
    | _ -> fail ()

[<Theory>]
[<InlineData("\"test\"")>]
[<InlineData("2")>]
[<InlineData("2.1")>]
[<InlineData("true")>]
[<InlineData("\"012-04-23T18:25:43.511Z\"")>]
[<InlineData("\"f27a5b7f-0b7c-4e79-aff5-bdb0d34f3a9f\"")>]
let ``Should fail if primitives present when array is root object`` value =
    let input = sprintf "[%s]" value

    let result = generateRecords FsharpCommon.fixName "Root" FsharpCommon.listGenerator input
    
    match result with
    | Ok _ -> fail ()
    | Error _ -> pass ()

[<Fact>]
let ``Should support object when array is root object`` () =
    let input = "[{}]"

    let result = generateRecords FsharpCommon.fixName "Root" FsharpCommon.listGenerator input
    
    match result with
    | Ok [ { Name = "Root"; Fields = [] } ] -> pass ()
    | _ -> fail ()

[<Fact>]
let ``Should support arrays when array is root object`` () =
    let input = "[[]]"

    let result = generateRecords FsharpCommon.fixName "Root" FsharpCommon.listGenerator input
    
    match result with
    | Ok [ { Name = "Root"; Fields = [] } ] -> pass ()
    | _ -> fail ()

[<Fact>]
let ``Should ignore null in array root object`` () =
    let input = "[{}, null]"

    let result = generateRecords FsharpCommon.fixName "Root" FsharpCommon.listGenerator input
    
    match result with
    | Ok [ { Name = "Root"; Fields = [] } ] -> pass ()
    | _ -> fail ()

[<Fact>]
let ``Should ignore undefined in array root object`` () =
    let input = "[{}, null]"

    let result = generateRecords FsharpCommon.fixName "Root" FsharpCommon.listGenerator input
    
    match result with
    | Ok [ { Name = "Root"; Fields = [] } ] -> pass ()
    | _ -> fail ()

[<Fact>]
let ``Should merge types in array root`` () =
    let input = @"[{""a"": 2}, {""b"": ""str""}]"

    let result = generateRecords FsharpCommon.fixName "Root" FsharpCommon.listGenerator input
    
    match result with
    | Ok [ { Name = "Root"; Fields = [{ TypeId = _; Name = "A"; Type = "int64"; Template = "%s option" }
                                      { TypeId = _; Name = "B"; Type = "string"; Template = "%s option" }
                                      ] } ] -> pass ()
    | _ -> fail ()

[<Fact>]
let ``Should aggregate types with same fields in array root`` () =
    let input = @"[{""a"": 2}, {""a"": 5}]"

    let result = generateRecords FsharpCommon.fixName "Root" FsharpCommon.listGenerator input
    
    match result with
    | Ok [ { Name = "Root"; Fields = [{ TypeId = _; Name = "A"; Type = "int64"; Template = "%s" } ] } ] -> pass ()
    | _ -> fail ()


[<Fact>]
let ``Should ignore null and undefined when aggregate types in array root`` () =
    let input = @"[{""a"": 2}, null, undefined, {""a"": 5}]"

    let result = generateRecords FsharpCommon.fixName "Root" FsharpCommon.listGenerator input
    
    match result with
    | Ok [ { Name = "Root"; Fields = [{ TypeId = _; Name = "A"; Type = "int64"; Template = "%s" } ] } ] -> pass ()
    | _ -> fail ()

[<Fact>]
let ``Should create empty object when aggregate unmatched types in array root`` () =
    let input = "[{}, []]"

    let result = generateRecords FsharpCommon.fixName "Root" FsharpCommon.listGenerator input
    
    match result with
    | Ok [ { Name = "Root"; Fields = [] } ] -> pass ()
    | _ -> fail ()

[<Fact>]
let ``Should ignore null and undefined when merge two diffrent types in array root`` ()  =
    let input = "[{}, null, [], undefined]"

    let result = generateRecords FsharpCommon.fixName "Root" FsharpCommon.listGenerator input
    
    match result with
    | Ok [ { Name = "Root"; Fields = [] } ] -> pass ()
    | _ -> fail ()

[<Fact>]
let ``Should extract object from nested arrays in array root`` () =
    let input = "[[{}]]"

    let result = generateRecords FsharpCommon.fixName "Root" FsharpCommon.listGenerator input
    
    match result with
    | Ok [ { Name = "Root"; Fields = [] } ] -> pass ()
    | _ -> fail ()