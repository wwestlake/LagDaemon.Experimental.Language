(*
    Yadsl (Yet Another Domain Specific Language)
    Copyright (C) 2019, William W. Westlake
    Licensed un the MIT License
*)

namespace LagDaemon.Yadsl.Compiler



module Model=

    open System
    open FParsec

    type TNumber =
        | TInteger of int
        | TFloat of float
        | TByte of byte
        
    type TFunction = {
        name: string
        parameters: (string * TType) list
        returnType: TType
    }

    and TStructure = {
        name: string
        fields: (string * TType) list 
    }

    and TType =
        | TChar of char
        | TString of string
        | TNumber of TNumber
        | TBoolean of bool
        | TStructure of TStructure
        | TUnit

    and TValue = TValue of TType

    type AbstractSyntaxTree =
        | YValue of TValue
        | YSymbol of string * TType
        | YStructure of TStructure
        | YFunction of TFunction

    type Keywords =
        | KW_Module
        | KW_Function
        | KW_If
        | KW_Then
        | KW_Else
        | KW_Do
        | KW_While
        | KW_For
        | KW_In
        | KW_With
        | KW_Define
        | KW_Let
        | KW_Lambda
        | KW_Type
        | KW_Recursive
        | KW_Import
        | KW_ForAll
        | KW_Where
        | KW_Test
        | KW_Spec
        | KW_Fact
        | KW_Property
        | KW_Expect
        | KW_Require
        | KW_Option
        | KW_Always
        | KW_Never
        | KW_May
        | KW_Must
        | KW_Shall
        | KW_Int
        | KW_Float
        | KW_String
        | KW_Char
        | KW_Bool
        | KW_Structure
        | KW_Byte
        | KW_Unit
        | OP_RightArrow
        | OP_LeftArrow
        | OP_DoubleRightArrow
        | OP_DoubleLeftArrow
        | OP_LessThan
        | OP_GreaterThan
        | OP_LessThanOrEqual
        | OP_GreaterThanOrEqual
        | OP_NotEqual
        | OP_Equal
        | OP_Modulus
        | OP_Power
        | OP_And
        | OP_Or
        | OP_Not
        | OP_Xor
        | OP_BW_And
        | OP_BW_Or
        | OP_BW_Not
        | OP_BW_Xor
        | OP_Add
        | OP_Subract
        | OP_Multiply
        | OP_Divide
        | SYM_Colon
        | SYM_SemiColon
        | SYM_Comma
        | SYM_Dot
        | SYM_DotDot
        | SYM_OpenParen
        | SYM_CloseParen
        | SYM_OpenBracket
        | SYM_CloseBracket
        | SYM_OpenBrace
        | SYM_CloseBrace
        | SYM_SingleQuote
        | SYM_DoubleQuote
        | SYM_BackTick
        | SYM_At
        | SYM_Bang
        | SYM_QuestionMark
        | SYM_BackSlant
        

    type UserState = unit // doesn't have to be unit, of course
    type Parser<'t> = Parser<'t, UserState>
