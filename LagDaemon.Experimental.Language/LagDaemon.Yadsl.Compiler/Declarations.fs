namespace LagDaemon.Yadsl.Compiler

module Declarations =
    open FParsec
    open Model
    open IdentifierParsers
    open KeywordParser
    open PrimParsers

    let scalarDecl : Parser<_> = parse {
        let! _ = pDefine
        let! ident = pidentifier
        return ident    
    }

    let typeDecl : Parser<_> = parse {
        let! _ = pColon
        let tp = pTypeSpec
        return! tp    
    }

    let scalarVariableDecl' : Parser<_> = parse {
        let! ident = scalarDecl
        let! tp = typeDecl
        return ScalarVariableDeclaration (tp, ident)
    }

    let inferredVariableDecl : Parser<_> = parse {
        let! ident = scalarDecl
        return InferredVariableDeclaration (Inferred, ident)
    }


    let scalarVariableDecl : Parser<_> = 
        scalarVariableDecl' <?|> inferredVariableDecl



    let functionDecl : Parser<_> = parse {
        let! _ = pFunction
        let! ident = pidentifier
        let! _ = pColon
        let! fspec = sepBy pTypeSpec pRightArrow
        return AbstractFunctionDeclaration (ident, fspec) 
    }
        
    let moduleDecl : Parser<_> = parse {
        let! _ = pModule
        let! module_name = sepBy pidentifier pDot
        let! export_list = between pOpenParen pCloseParen (sepBy pidentifier pComma)
        return ModuleHeader (module_name, export_list)
    }


