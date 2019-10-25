// Learn more about F# at http://fsharp.org

open LagDaemon.Yadsl.Compiler
open ValueParsers
open Executive
open System
open FParsec
open Model
open PrimParsers
open IdentifierParsers

let test : Parser<_> =
    hexLiteral .>> eof


[<EntryPoint>]
let main argv =
    
    execString pnumber "42"
    execString pnumber "3.14e29"


    execString test "0x1111"
    execString literalValue "true"
    execString literalValue "false"
    execString literalValue "23u"
    execString literalValue "23b"
    execString literalValue "\"String literal\twithtabs\" "
    execString literalValue "'\t'"
    execString literalValue "4"
    execString literalValue "44.2"
    execString literalValue "0x1234"

    let comments = @"(*
        block comment
        {*
            nested block comment
        *)
    *)
    
    
    "



    execString pcomment comments

    execString pidentifier "_AnIdentifier01"
    execString pidentifier "9_AnInvalidIdentifier01"
    execString pidentifier "data"

    0 // return an integer exit code

