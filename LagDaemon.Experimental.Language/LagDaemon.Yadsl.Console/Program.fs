// Learn more about F# at http://fsharp.org

open LagDaemon.Yadsl.Compiler
open ValueParsers
open Executive
open System
open FParsec
open Model
open PrimParsers
open IdentifierParsers
open Expressions
open Declarations

let test : Parser<_> =
    hexLiteral .>> eof


[<EntryPoint>]
let main argv =

    execString scalarVariableDecl "def myident : int"
    execString scalarVariableDecl "def myident2"
    0 // return an integer exit code

