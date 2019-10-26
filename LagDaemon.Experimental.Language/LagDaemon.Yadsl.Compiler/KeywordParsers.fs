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
    let pModule     : (Parser<_>) = str_ws  "module"                          >>% KWModule
    let pIf         : (Parser<_>) = str_ws  "if"                              >>% KWIf
    let pThen       : (Parser<_>) = str_ws  "then"                            >>% KWThen
    let pElse       : (Parser<_>) = str_ws  "else"                            >>% KWElse
    let pDo         : (Parser<_>) = str_ws  "do"                              >>% KWDo
    let pWhile      : (Parser<_>) = str_ws  "while"                           >>% KWWhile
    let pFor        : (Parser<_>) = str_ws  "for"                             >>% KWFor
    let pPin        : (Parser<_>) = str_ws  "in"                              >>% KWIn
    let pWith       : (Parser<_>) = str_ws  "with"                            >>% KWWith
    let pDefine     : (Parser<_>) = str_ws  "define" <|> str_ws "def"         >>% KWDefine
    let pLet        : (Parser<_>) = str_ws  "let"                             >>% KWLet
    let pLambda     : (Parser<_>) = str_ws  "lambda" <|> str_ws "\\"          >>% KWLambda
    let pRecursive  : (Parser<_>) = str_ws  "rec"                             >>% KWRec
    let pImport     : (Parser<_>) = str_ws  "import" <|> str_ws "imp"         >>% KWImport
    let pForAll     : (Parser<_>) = str_ws  "forall" <|> str_ws "foreach"     >>% KWForall
    let pWhere      : (Parser<_>) = str_ws  "where"                           >>% KWWhere
    let pTest       : (Parser<_>) = str_ws  "test"                            >>% KWTest
    let pSpec       : (Parser<_>) = str_ws  "specification" <|> str_ws "spec" >>% KWSpecification
    let pFact       : (Parser<_>) = str_ws  "fact"                            >>% KWFact
    let pProperty   : (Parser<_>) = str_ws  "property" <|> str_ws "prop"      >>% KWProperty
    let pExpect     : (Parser<_>) = str_ws  "expect"                          >>% KWExpect
    let pRequire    : (Parser<_>) = str_ws  "require"                         >>% KWRequire
    let pOption     : (Parser<_>) = str_ws  "option" <|> str_ws "opt"         >>% KWOption
    let pAlways     : (Parser<_>) = str_ws  "always"                          >>% KWAlways
    let pNever      : (Parser<_>) = str_ws  "never"                           >>% KWNever
    let pMay        : (Parser<_>) = str_ws  "may"                             >>% KWMay
    let pMust       : (Parser<_>) = str_ws  "must"                            >>% KWMust
    let pShall      : (Parser<_>) = str_ws  "shall"                           >>% KWShall

    
    let pUnit                      : Parser<_> = str_ws  "unit"             >>% Unit
    let pBoolean                   : Parser<_> = str_ws  "bool"             >>% Boolean
    let pInteger                   : Parser<_> = str_ws  "int"              >>% Integer
    let pFloat                     : Parser<_> = str_ws  "float"            >>% FloatNum
    let pString                    : Parser<_> = str_ws  "string"           >>% String
    let pCharacter                 : Parser<_> = str_ws  "char"             >>% Character
    let pByte                      : Parser<_> = str_ws  "byte"             >>% Byte
    let pClass                     : Parser<_> = str_ws  "class"            >>% Class
    let pStruct                    : Parser<_> = str_ws  "struct"           >>% Struct
    let pUnion                     : Parser<_> = str_ws  "union"            >>% Union
    let pType                      : Parser<_> = str_ws  "type"             >>% Type
    let pAdtProduct                : Parser<_> = str_ws  "record"           >>% AdtProduct
    let pAdtTypeEnumeration        : Parser<_> = str_ws  "discr"            >>% AdtTypeEnumeration
    let pAdtScalarEnumberation     : Parser<_> = str_ws  "enum"             >>% AdtScalarEnumberation
    let pInterface                 : Parser<_> = str_ws  "interface"        >>% Interface
    let pFunction                  : Parser<_> = str_ws  "function"         >>% Function

    let pTypeSpec : Parser<_> = choice [
                pUnit                 
                pBoolean              
                pInteger              
                pFloat                
                pString               
                pCharacter            
                pByte                 
                pClass                
                pStruct               
                pUnion                
                pType                 
                pAdtProduct           
                pAdtTypeEnumeration   
                pAdtScalarEnumberation
                pInterface            
                pFunction             
            ]

    // operators
    let pRightArrow         : (Parser<_>) = str_ws  "->"                    >>% OPRightArrow
    let pLeftArrow          : (Parser<_>) = str_ws  "<-"                    >>% OPLeftArrow
    let pDoubleRightArrow   : (Parser<_>) = str_ws  "=>>"                   >>% OPDoubleRightArrow
    let pDoubleLeftArrow    : (Parser<_>) = str_ws  "<<="                   >>% OPDoubleLeftArrow
    let pEquals             : (Parser<_>) = str_ws  "="                     >>% OPEquals
    let pLessThan           : (Parser<_>) = str_ws  "<"                     >>% OPLessThan
    let pGreaterThan        : (Parser<_>) = str_ws  ">"                     >>% OPGreaterThan
    let pLessThanOrEqual    : (Parser<_>) = str_ws  "<="                    >>% OPLessThanOrEqual
    let pGreaterThanOrEqual : (Parser<_>) = str_ws  ">="                    >>% OPGreaterThanOrEqual
    let pNotEqual           : (Parser<_>) = str_ws  "<>"                    >>% OPNotEqual
    let pModulus            : (Parser<_>) = str_ws  "%"                     >>% OPModulus
    let pPower              : (Parser<_>) = str_ws  "^"                     >>% OPPower
    let pOr                 : (Parser<_>) = str_ws  "||"                    >>% OPOr
    let pAnd                : (Parser<_>) = str_ws  "&&"                    >>% OPAnd
    let pNot                : (Parser<_>) = str_ws  "not" <|> str_ws "!!"   >>% OPNot
    let pXor                : (Parser<_>) = str_ws  "~~"                    >>% OPXor
    let pBwAnd              : (Parser<_>) = str_ws  "&&&"                   >>% OPBwAnd
    let pBwOr               : (Parser<_>) = str_ws  "|||"                   >>% OPBwOr
    let pBwNot              : (Parser<_>) = str_ws  "!!!"                   >>% OPBwNot
    let pBwXor              : (Parser<_>) = str_ws  "~"                     >>% OPBwXor
    let pAdd                : (Parser<_>) = str_ws  "+"                     >>% OPAdd
    let pSubtract           : (Parser<_>) = str_ws  "-"                     >>% OPSubtract
    let pMultiply           : (Parser<_>) = str_ws  "*"                     >>% OPMultiply
    let pDivide             : (Parser<_>) = str_ws  "/"                     >>% OPDivide
    let pColon              : (Parser<_>) = str_ws  ":"                     >>% OPColon
    let pSemicolon          : (Parser<_>) = str_ws  ";"                     >>% OPSemicolon
    let pComma              : (Parser<_>) = str_ws  ","                     >>% OPComma
    let pDot                : (Parser<_>) = str_ws  "."                     >>% OPDot
    let pDotDot             : (Parser<_>) = str_ws  ".."                    >>% OPDotDot
    let pOpenParen          : (Parser<_>) = str_ws  "("                     >>% OPOpenParen
    let pCloseParen         : (Parser<_>) = str_ws  ")"                     >>% OPCloseParen
    let pOpenBracket        : (Parser<_>) = str_ws  "["                     >>% OPOpenBracket
    let pCloseBracket       : (Parser<_>) = str_ws  "]"                     >>% OPCloseBracket
    let pOpenBrace          : (Parser<_>) = str_ws  "{"                     >>% OPOpenBrace
    let pCloseBrace         : (Parser<_>) = str_ws  "}"                     >>% OPCloseBrace
    let pSingleQuote        : (Parser<_>) = str_ws  "'"                     >>% OPSingleQuote
    let pDoubleQuote        : (Parser<_>) = str_ws  "\""                    >>% OPDoubleQuote
    let pBackTick           : (Parser<_>) = str_ws  "`"                     >>% OPBackTick
    let pAt                 : (Parser<_>) = str_ws  "@"                     >>% OPAt
    let pBang               : (Parser<_>) = str_ws  "!"                     >>% OPBang
    let pQuestionMark       : (Parser<_>) = str_ws  "?"                     >>% OPQuestionMark
    let pBackSlant          : (Parser<_>) = str_ws  "\\"                    >>% OPBackSlant


    






