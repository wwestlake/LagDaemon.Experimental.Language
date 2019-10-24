// Learn more about F# at http://fsharp.org

open LagDaemon.Yadsl.Compiler
open ValueParsers
open Executive
open System
open FParsec
open Model

let test : Parser<_> =
    hexLiteral .>> eof


[<EntryPoint>]
let main argv =
    //execString test "0x1111"
    execString literalValue "true "
    execString literalValue "false "
    execString literalValue "23u "
    execString literalValue "23b "
    execString literalValue "\"String literal\twithtabs\" "
    execString literalValue "'\t' "
    execString literalValue "4 "
    execString literalValue "44.2 "
    execString literalValue "0x1234 "
    0 // return an integer exit code

