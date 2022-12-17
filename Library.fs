module ProtoExp

open System
open NUnit.Framework

#nowarn "40"

let inline (^) f x = f(x)

let isProtoVersionSupported = function
    | "proto2"
    | "proto3" ->
        true
    | _ ->
        false

type Field = {
    Position: uint voption
    Type: Type
    Rule: FieldRule
}

and Type =
    | Message of Name: string * Fields: Field * OtherTypes: Type[]
    | Reserved of ReservedParams
    | Enum of EnumField array * allowAlias: bool voption

and EnumField = {
    Name: string
    Position: uint
}

and ScalarType =
    | Double
    | Float
    | Int32
    | Int64
    | UInt32
    | UInt64
    | SInt32
    | SInt64
    | Fixed32
    | Fixed64
    | SFixed32
    | SFixed64
    | Bool
    | String
    | Bytes

and CommentType =
    | SingleLine of text: string
    | MultiLine of text: string

and ReservedParams =
    | FieldNumbers of FieldNumberValue array
    | FieldNames of string array

and FieldNumberValue =
    | Number of int
    | Range of NumberValue * NumberValue

and NumberValue =
    | Value of int
    | Max

and FieldRule =
    | Singular
    | Optional
    | Repeated
    | Map

// let [<Literal>] protoFileName = "sample_of.proto"
// let protoFileBody = System.IO.File.ReadAllText protoFileName

open FParsec

let pProtoVersion =
    let x = pstring "syntax"
    ()

let test p str =
    match run p str with
    | Success(result, a, b)   ->
        Assert.Pass($"Success: {result} {a} {b}")
    | Failure(errorMsg, _, _) ->
        Assert.Fail($"Failure: {errorMsg}")

let getResult p str =
    match run p str with
    | Success (result, _, _) ->
        result
    | Failure _ ->
        None


let [<Test>] ``proto version works properly`` () =
    let pProtoVersion =
        let pDQuote = pchar '"'
        let letterOrDigit = asciiLetter <|> digit
        let pProtoVersion = manyChars letterOrDigit
        let pSyntax =
            skipString "syntax"
            .>> optional spaces
            .>> pchar '='
            .>> optional spaces
            >>. between pDQuote pDQuote pProtoVersion
            .>> pchar ';'
        opt pSyntax
    
    let res1 = getResult pProtoVersion "syntax = \"proto3\";"
    Assert.AreEqual (Option.get res1, "proto3")
    
    let res2 = getResult pProtoVersion "syntax = \"proto2\";"
    Assert.AreEqual (Option.get res2, "proto2")
    
    let res3 = getResult pProtoVersion ""
    Assert.AreEqual (res3, None)

let [<Test>] ``parse comment``() =
    let pComment =
        let anySymbol = satisfy ^ fun c -> (c = '\n' || c = '\r') |> not
        pstring "//" >>. manyChars anySymbol |> opt
    let comment = $"// Pew pew pewpew! {Environment.NewLine}Hi"
    let res = getResult pComment comment
    Assert.AreEqual (Option.get res, " Pew pew pewpew! ")

let [<Test>] ``parse message`` () =
    let message = """
message Inne1r                {
      int64 ival = 1;
      bool  booly = 2;
    }
"""
    
    let pMessage =
        let pMessage = pstring "message"
        let letterOrDigit = letter <|> digit
        let spacesAndNewLines =
            optional spaces
            .>> optional newline
            .>> optional spaces
        let pFirstLetterThenLetterOrDigit = many1Chars2 asciiLetter letterOrDigit
        let pField =
            spacesAndNewLines
            >>. pFirstLetterThenLetterOrDigit
            .>> optional spaces
            .>>. pFirstLetterThenLetterOrDigit
            .>> optional spaces
            .>> pchar '='
            .>> optional spaces
            .>>. manyChars digit
            .>> pchar ';'
            .>> spacesAndNewLines
        
        (manyChars newline >>. optional spaces)
        >>. pMessage
        >>. optional spaces
        >>. pFirstLetterThenLetterOrDigit
        .>> spacesAndNewLines
        .>> pchar '{'
        .>>. many pField
        .>> pchar '}'
    test pMessage message

type TMessageName = string
type TMessageFieldType = string
type TMessageFieldPosition = string
type TMessageField = (TMessageFieldType * TMessageName) * TMessageFieldPosition
type TMessage = | Message of TMessage list option * TMessageField list option

let [<Test>] ``parse nested messages`` () =
    let message = """
 message Root {

      int32 ival = 1;
      bool  booly = 2;
      message Inner {

          int32 ival = 1;
          bool  booly = 2;
    }
}
"""
    let spacesAndNewLines =
        optional spaces
        .>> optional newline
        .>> optional spaces
    
    let pMessageStr = pstring "message"
    let letterOrDigit = letter <|> digit
    let pFirstLetterThenLetterOrDigit = many1Chars2 asciiLetter letterOrDigit
    let pField : Parser<TMessageField, unit> =
        spacesAndNewLines
        >>. pFirstLetterThenLetterOrDigit
        .>> optional spaces
        .>>. pFirstLetterThenLetterOrDigit
        .>> optional spaces
        .>> pchar '='
        .>> optional spaces
        .>>. manyChars digit
        .>> pchar ';'
        .>> spacesAndNewLines

    let pMessage, pMessageRef = createParserForwardedToRef()
    pMessageRef.Value <-
       (manyChars newline >>. optional spaces)
        >>. pMessageStr
        >>. optional spaces
        >>. pFirstLetterThenLetterOrDigit
        .>> spacesAndNewLines
        .>> pchar '{'
        .>>. opt (many pField)
        .>> pchar '}'
        .>> spacesAndNewLines
    
    test pMessage message

// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

