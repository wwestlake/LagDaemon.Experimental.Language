﻿namespace LagDaemon.Yadsl.Compiler

module KeywordParser =
    
    open FParsec
    open FParsec.Pipes
    open Model
    open PrimParsers



    // Keywords
    let pModule     : (Parser<_>) = str "module" 
    let pFunction   : (Parser<_>) = str "fun" <|> str "function"
    let pIf         : (Parser<_>) = str "if"
    let pThen       : (Parser<_>) = str "then"
    let pElse       : (Parser<_>) = str "else"
    let pDo         : (Parser<_>) = str "do"
    let pWhile      : (Parser<_>) = str "while"
    let pFor        : (Parser<_>) = str "for"
    let pPin         : (Parser<_>) = str "in"
    let pWith       : (Parser<_>) = str "with"
    let pDefine     : (Parser<_>) = str "define" <|> str "def"
    let pLet        : (Parser<_>) = str "let"
    let pLambda     : (Parser<_>) = str "lambda" <|> str "\\" 
    let pType       : (Parser<_>) = str "type"
    let pRecursive  : (Parser<_>) = str "rec"
    let pImport     : (Parser<_>) = str "import" <|> str "imp"
    let pForAll     : (Parser<_>) = str "forall" <|> str "foreach"
    let pWhere      : (Parser<_>) = str "where"
    let pTest       : (Parser<_>) = str "test"
    let pSpec       : (Parser<_>) = str "specification" <|> str "spec"
    let pFact       : (Parser<_>) = str "fact"
    let pProperty   : (Parser<_>) = str "property" <|> str "prop"
    let pExpect     : (Parser<_>) = str "expect"
    let pRequire    : (Parser<_>) = str "require"
    let pOption     : (Parser<_>) = str "option" <|> str "opt"
    let pAlways     : (Parser<_>) = str "always"
    let pNever      : (Parser<_>) = str "never"
    let pMay        : (Parser<_>) = str "may"
    let pMust       : (Parser<_>) = str "must"
    let pShall      : (Parser<_>) = str "shall"
    let pInt        : (Parser<_>) = str "integer" <|> str "int"
    let pFloat      : (Parser<_>) = str "float"
    let pString     : (Parser<_>) = str "string"
    let pChar       : (Parser<_>) = str "char"
    let pBool       : (Parser<_>) = str "bool"
    let pStruct     : (Parser<_>) = str "record" <|> str "structure" <|> str "struct"
    let pByte       : (Parser<_>) = str "byte"
    let pUnit       : (Parser<_>) = str "unit"

    // operators
    let pRightArrow         : (Parser<_>) = str "->"
    let pLeftArrow          : (Parser<_>) = str "<-"
    let pDoubleRightArrow   : (Parser<_>) = str "=>>"
    let pDoubleLeftArrow    : (Parser<_>) = str "<<="
    let pLessThan           : (Parser<_>) = str "<"
    let pGreaterThan        : (Parser<_>) = str ">"
    let pLessThanOrEqual    : (Parser<_>) = str "<="
    let pGreaterThanOrEqual : (Parser<_>) = str ">="
    let pNotEqual           : (Parser<_>) = str "<>"
    let pModulus            : (Parser<_>) = str "%"
    let pPower              : (Parser<_>) = str "^"
    let pOr                 : (Parser<_>) = str "||"
    let pAnd                : (Parser<_>) = str "&&"
    let pNot                : (Parser<_>) = str "not" <|> str "!!"
    let pXor                : (Parser<_>) = str "~~"
    let pBwAnd              : (Parser<_>) = str "&"
    let pBwOr               : (Parser<_>) = str "|"
    let pBwNot              : (Parser<_>) = str "!"
    let pBwXor              : (Parser<_>) = str "~"
    let pAdd                : (Parser<_>) = str "+"
    let pSubtract           : (Parser<_>) = str "-"
    let pMultiply           : (Parser<_>) = str "*"
    let pDivide             : (Parser<_>) = str "/"
    let pColon              : (Parser<_>) = str ":"
    let pSemicolon          : (Parser<_>) = str ";"
    let pComma              : (Parser<_>) = str ","
    let pDot                : (Parser<_>) = str "."
    let pDotDot             : (Parser<_>) = str ".."
    let pOpenParen          : (Parser<_>) = str "("
    let pCloseParen         : (Parser<_>) = str ")"
    let pOpenBracket        : (Parser<_>) = str "["
    let pCloseBracket       : (Parser<_>) = str "]"
    let pOpenBrace          : (Parser<_>) = str "{"
    let pCloseBrace         : (Parser<_>) = str "}"
    let pSingleQuote        : (Parser<_>) = str "'"
    let pDoubleQuote        : (Parser<_>) = str "\""
    let pBackTick           : (Parser<_>) = str "`"
    let pAt                 : (Parser<_>) = str "@"
    let pBang               : (Parser<_>) = str "!"
    let pQuestionMark       : (Parser<_>) = str "?"
    let pBackSlant          : (Parser<_>) = str "\\"









