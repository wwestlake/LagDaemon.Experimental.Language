namespace LagDaemon.Yadsl.Compiler

module KeywordParser =
    
    open FParsec
    open FParsec.Pipes
    open Model
    open PrimParsers

    let keywords = [
        "module" 
        "fun" 
        "function"
        "if"
        "then"
        "else"
        "do"
        "while"
        "for"
        "in"
        "with"
        "define"
        "def"
        "let"
        "lambda"
        "\\" 
        "type"
        "rec"
        "import"
        "imp"
        "forall"
        "foreach"
        "where"
        "test"
        "specification"
        "spec"
        "fact"
        "property"
        "prop"
        "expect"
        "require"
        "option"
        "opt"
        "always"
        "never"
        "may"
        "must"
        "shall"
        "integer"
        "int"
        "float"
        "string"
        "char"
        "bool"
        "record"
        "structure"
        "struct"
        "byte"
        "data"
        "unit"
    ]

    let isKeyword str = keywords |> List.contains str

    // Keywords
    let pModule     : (Parser<_>) = str "module"                        >>% KWModule
    let pFunction   : (Parser<_>) = str "fun" <|> str "function"        >>% KWFunction
    let pIf         : (Parser<_>) = str "if"                            >>% KWIf
    let pThen       : (Parser<_>) = str "then"                          >>% KWThen
    let pElse       : (Parser<_>) = str "else"                          >>% KWElse
    let pDo         : (Parser<_>) = str "do"                            >>% KWDo
    let pWhile      : (Parser<_>) = str "while"                         >>% KWWhile
    let pFor        : (Parser<_>) = str "for"                           >>% KWFor
    let pPin         : (Parser<_>) = str "in"                           >>% KWIn
    let pWith       : (Parser<_>) = str "with"                          >>% KWWith
    let pDefine     : (Parser<_>) = str "define" <|> str "def"          >>% KWDefine
    let pLet        : (Parser<_>) = str "let"                           >>% KWLet
    let pLambda     : (Parser<_>) = str "lambda" <|> str "\\"           >>% KWLambda
    let pType       : (Parser<_>) = str "type"                          >>% KWType
    let pRecursive  : (Parser<_>) = str "rec"                           >>% KWRec
    let pImport     : (Parser<_>) = str "import" <|> str "imp"          >>% KWImport
    let pForAll     : (Parser<_>) = str "forall" <|> str "foreach"      >>% KWForall
    let pWhere      : (Parser<_>) = str "where"                         >>% KWWhere
    let pTest       : (Parser<_>) = str "test"                          >>% KWTest
    let pSpec       : (Parser<_>) = str "specification" <|> str "spec"  >>% KWSpecification
    let pFact       : (Parser<_>) = str "fact"                          >>% KWFact
    let pProperty   : (Parser<_>) = str "property" <|> str "prop"       >>% KWProperty
    let pExpect     : (Parser<_>) = str "expect"                        >>% KWExpect
    let pRequire    : (Parser<_>) = str "require"                       >>% KWRequire
    let pOption     : (Parser<_>) = str "option" <|> str "opt"          >>% KWOption
    let pAlways     : (Parser<_>) = str "always"                        >>% KWAlways
    let pNever      : (Parser<_>) = str "never"                         >>% KWNever
    let pMay        : (Parser<_>) = str "may"                           >>% KWMay
    let pMust       : (Parser<_>) = str "must"                          >>% KWMust
    let pShall      : (Parser<_>) = str "shall"                         >>% KWShall
    let pInt        : (Parser<_>) = str "integer" <|> str "int"         >>% KWInteger
    let pFloat      : (Parser<_>) = str "float"                         >>% KWFloat
    let pString     : (Parser<_>) = str "string"                        >>% KWString
    let pChar       : (Parser<_>) = str "char"                          >>% KWChar
    let pBool       : (Parser<_>) = str "bool"                          >>% KWBool
    let pStruct     : (Parser<_>) = str "record" <|> str "structure" 
                                                       <|> str "struct" >>% KWRecord
    let pByte       : (Parser<_>) = str "byte"                          >>% KWByte
    let pData       : (Parser<_>) = str "data"                          >>% KWData
    let pUnit       : (Parser<_>) = str "unit"                          >>% KWUnit

    // operators
    let pRightArrow         : (Parser<_>) = str "->"                    >>% OPRightArrow
    let pLeftArrow          : (Parser<_>) = str "<-"                    >>% OPLeftArrow
    let pDoubleRightArrow   : (Parser<_>) = str "=>>"                   >>% OPDoubleRightArrow
    let pDoubleLeftArrow    : (Parser<_>) = str "<<="                   >>% OPDoubleLeftArrow
    let pEquals             : (Parser<_>) = str "="                     >>% OPEquals
    let pLessThan           : (Parser<_>) = str "<"                     >>% OPLessThan
    let pGreaterThan        : (Parser<_>) = str ">"                     >>% OPGreaterThan
    let pLessThanOrEqual    : (Parser<_>) = str "<="                    >>% OPLessThanOrEqual
    let pGreaterThanOrEqual : (Parser<_>) = str ">="                    >>% OPGreaterThanOrEqual
    let pNotEqual           : (Parser<_>) = str "<>"                    >>% OPNotEqual
    let pModulus            : (Parser<_>) = str "%"                     >>% OPModulus
    let pPower              : (Parser<_>) = str "^"                     >>% OPPower
    let pOr                 : (Parser<_>) = str "||"                    >>% OPOr
    let pAnd                : (Parser<_>) = str "&&"                    >>% OPAnd
    let pNot                : (Parser<_>) = str "not" <|> str "!!"      >>% OPNot
    let pXor                : (Parser<_>) = str "~~"                    >>% OPXor
    let pBwAnd              : (Parser<_>) = str "&&&"                   >>% OPBwAnd
    let pBwOr               : (Parser<_>) = str "|||"                   >>% OPBwOr
    let pBwNot              : (Parser<_>) = str "!!!"                   >>% OPBwNot
    let pBwXor              : (Parser<_>) = str "~"                     >>% OPBwXor
    let pAdd                : (Parser<_>) = str "+"                     >>% OPAdd
    let pSubtract           : (Parser<_>) = str "-"                     >>% OPSubtract
    let pMultiply           : (Parser<_>) = str "*"                     >>% OPMultiply
    let pDivide             : (Parser<_>) = str "/"                     >>% OPDivide
    let pColon              : (Parser<_>) = str ":"                     >>% OPColon
    let pSemicolon          : (Parser<_>) = str ";"                     >>% OPSemicolon
    let pComma              : (Parser<_>) = str ","                     >>% OPComma
    let pDot                : (Parser<_>) = str "."                     >>% OPDot
    let pDotDot             : (Parser<_>) = str ".."                    >>% OPDotDot
    let pOpenParen          : (Parser<_>) = str "("                     >>% OPOpenParen
    let pCloseParen         : (Parser<_>) = str ")"                     >>% OPCloseParen
    let pOpenBracket        : (Parser<_>) = str "["                     >>% OPOpenBracket
    let pCloseBracket       : (Parser<_>) = str "]"                     >>% OPCloseBracket
    let pOpenBrace          : (Parser<_>) = str "{"                     >>% OPOpenBrace
    let pCloseBrace         : (Parser<_>) = str "}"                     >>% OPCloseBrace
    let pSingleQuote        : (Parser<_>) = str "'"                     >>% OPSingleQuote
    let pDoubleQuote        : (Parser<_>) = str "\""                    >>% OPDoubleQuote
    let pBackTick           : (Parser<_>) = str "`"                     >>% OPBackTick
    let pAt                 : (Parser<_>) = str "@"                     >>% OPAt
    let pBang               : (Parser<_>) = str "!"                     >>% OPBang
    let pQuestionMark       : (Parser<_>) = str "?"                     >>% OPQuestionMark
    let pBackSlant          : (Parser<_>) = str "\\"                    >>% OPBackSlant









