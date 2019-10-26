(*
    Yadsl (Yet Another Domain Specific Language)
    Copyright (C) 2019, William W. Westlake
    Licensed un the MIT License
*)

namespace LagDaemon.Yadsl.Compiler



module Model=

    open System
    open FParsec

    type Program = Module list
    and Module = ModuleHeader * Declaration list
    and ModuleHeader = Identifier * Identifier list
    and Declaration =
        | StaticVariableDeclaration of VariableDeclaration
        | FunctionDeclaration of FunctionDeclaration
        | ClassDeclaration of ClassDeclaration
        | LambdaDeclaration of LambdaDeclaration
        | StructDeclaration of StructDeclaration
        | UnionDeclaraion of UnionDeclaration
        | InterfaceDeclaration of InterfaceDeclaration

    and AdtProductDeclaration = Identifier * VariableDeclaration list * FunctionDeclaration list
    and AdtProductAbstractDeclaration = Identifier * AbstractFunctionDeclaration list
    and ClassDeclaration = AdtProductDeclaration
    and StructDeclaration = AdtProductDeclaration
    and UnionDeclaration = AdtProductDeclaration
    and InterfaceDeclaration = AdtProductAbstractDeclaration
    and AdtSumDeclaration = Identifier * TypeSpec list
    and AdtTypeEnumationDeclarationh = AdtSumDeclaration
    and AdtScalarEnumerationDeclaration = AdtSumDeclaration

    and TypeSpec =
        | Inferred
        | Unit
        | Boolean
        | Integer
        | FloatNum
        | String
        | Character
        | Byte
        | Class
        | Struct
        | Union
        | Type
        | AdtProduct
        | AdtTypeEnumeration
        | AdtScalarEnumberation
        | Interface
        | Function
        

    and FunctionSpec = (Identifier * TypeSpec) list
        
    and VariableDeclaration =
        | InferredVariableDeclaration of TypeSpec * Identifier
        | ScalarVariableDeclaration of TypeSpec * Identifier
        | CollectionVariableDeclaration of TypeSpec * Identifier
        | TupleVariableDeclaration of (TypeSpec * Identifier) list

    and FunctionDeclaration = TypeSpec * Identifier * Parameters * CompoundStatement
    and AbstractFunctionDeclaration = TypeSpec * Identifier * Parameters
    and LambdaDeclaration = TypeSpec * CaptureList * Parameters * CompoundStatement
    and CaptureList = (TypeSpec * Identifier) list
    and Identifier = Identifier of string
    and Parameters = VariableDeclaration list
    and IdentifierRef = { Identifier: string }
    and Statement =
        | ExpressionStatement of ExpressionStatement
        | CompoundStatement of CompoundStatement
        | IfStatement of IfStatement
        | WhileStatement of WhileStatement
        | ReturnStatement of Expression option
        | BreakStatement
        | ContinueStatement
    and ExpressionStatement =
        | Expression of Expression
        | Nop
    and CompoundStatement = LocalDeclaration * Statement list
    and LocalDeclaration = VariableDeclaration list
    and IfStatement = Expression * Statement * Statement option
    and WhileStatement = Expression * Statement
    and Expression =
        | ScalarAssignmentExpression of IdentifierRef * Expression
        | ArrayAssignmentExpression of IdentifierRef * Expression * Expression
        | BinaryExpression of Expression * BinaryOperator * Expression
        | UnaryExpression of UnaryOperator * Expression
        | IdentifierExpression of IdentifierRef
        | ArrayIdentifierExpression of IdentifierRef * Expression
        | FunctionCallExpression of Identifier * Arguments
        | ArraySizeExpression of IdentifierRef
        | LiteralExpression of Literal
        | ArrayAllocationExpression of TypeSpec * Expression
    and BinaryOperator =
        | ConditionalOr
        | Equal
        | NotEqual
        | LessEqual
        | Less
        | GreaterEqual
        | Greater
        | ConditionalAnd
        | Add
        | Subtract
        | Multiply
        | Divide
        | Modulus

    and UnaryOperator =
        | LogicalNegate
        | Negate
        | Identity
    and Arguments = Expression list
    and Literal =
        | TInteger of int64
        | TLong of uint64
        | TFloat of float
        | TString of string
        | TBool of bool
        | TChar of char
        | TByte of byte

    type Keywords =
        | KWModule 
        | KWFunction
        | KWIf
        | KWThen
        | KWElse
        | KWDo
        | KWWhile
        | KWFor
        | KWIn
        | KWWith
        | KWDefine
        | KWLet
        | KWLambda
        | KWType
        | KWRec
        | KWImport
        | KWForall
        | KWWhere
        | KWTest
        | KWSpecification
        | KWFact
        | KWProperty
        | KWExpect
        | KWRequire
        | KWOption
        | KWAlways
        | KWNever
        | KWMay
        | KWMust
        | KWShall
        | KWInteger
        | KWInt
        | KWFloat
        | KWString
        | KWChar
        | KWBool
        | KWRecord
        | KWStructure
        | KWByte
        | KWData
        | KWUnit


    and Operators =
        | OPRightArrow         
        | OPLeftArrow          
        | OPDoubleRightArrow   
        | OPDoubleLeftArrow    
        | OPEquals             
        | OPLessThan           
        | OPGreaterThan        
        | OPLessThanOrEqual    
        | OPGreaterThanOrEqual 
        | OPNotEqual           
        | OPModulus            
        | OPPower              
        | OPOr                 
        | OPAnd                
        | OPNot                
        | OPXor                
        | OPBwAnd              
        | OPBwOr               
        | OPBwNot              
        | OPBwXor              
        | OPAdd                
        | OPSubtract           
        | OPMultiply           
        | OPDivide             
        | OPColon              
        | OPSemicolon          
        | OPComma              
        | OPDot                
        | OPDotDot             
        | OPOpenParen          
        | OPCloseParen         
        | OPOpenBracket        
        | OPCloseBracket       
        | OPOpenBrace          
        | OPCloseBrace         
        | OPSingleQuote        
        | OPDoubleQuote        
        | OPBackTick           
        | OPAt                 
        | OPBang               
        | OPQuestionMark       
        | OPBackSlant          



    and UserState = {
      symTable: Map<string, Literal>
    }
        with static member Default = { symTable = Map.empty }

    type Parser<'t> = Parser<'t, UserState>
