namespace LagDaemon.Yadsl.Compiler



module Parser = 
    open System.Text
    open System.IO
    open Model
    open Piglet
    open Piglet.Parser
    open ParserUtilities
    
    let configurator = ParserFactory.Configure<obj>()
    let nonTerminal<'T> () = new NonTerminalWrapper<'T>(configurator.CreateNonTerminal())
    
    let program                   = nonTerminal<Program>()
    let declarationList           = nonTerminal<Declaration list>()
    let declaration               = nonTerminal<Declaration>()
    let staticVariableDeclaration = nonTerminal<VariableDeclaration>()
    let functionDeclaration       = nonTerminal<FunctionDeclaration>()




