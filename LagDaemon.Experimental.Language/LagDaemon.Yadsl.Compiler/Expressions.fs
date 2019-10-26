namespace LagDaemon.Yadsl.Compiler


module Expressions =
    
    open FParsec
    open Model
    open IdentifierParsers
    open KeywordParser
    open PrimParsers

    let scalarVariableDecl : Parser<_> = parse {
        let! _ = pDefine
        let! _ = ws
        let! ident = pidentifier
        let! _ = ws
        let! _ = pColon
        let! _ = ws
        let! tp = pTypeSpec
        return ScalarVariableDeclaration (tp, ident)
    }
               
    
