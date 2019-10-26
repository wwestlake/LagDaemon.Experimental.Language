namespace LagDaemon.Yadsl.Compiler

module Declarations =
    open FParsec
    open Model
    open IdentifierParsers
    open KeywordParser
    open PrimParsers

    let scalarDecl : Parser<_> = parse {
        let! _ = ws
        let! _ = pDefine
        let! _ = ws
        let! ident = pidentifier
        return ident    
    }

    let typeDecl : Parser<_> = parse {
        let! _ = ws
        let! _ = pColon
        let! _ = ws
        let tp = pTypeSpec
        return! tp    
    }

    let scalarVariableDecl_full : Parser<_> = parse {
        let! ident = scalarDecl
        let! tp = typeDecl
        return ScalarVariableDeclaration (tp, ident)
    }

    let scalarVariableDecl_infered : Parser<_> = parse {
        let! ident = scalarDecl
        return ScalarVariableDeclaration (Inferred, ident)
    }

    let (<?|>) a b : Parser<_> =
        attempt a <|> b

    let scalarVariableDecl : Parser<_> = 
        scalarVariableDecl_full <?|> scalarVariableDecl_infered




