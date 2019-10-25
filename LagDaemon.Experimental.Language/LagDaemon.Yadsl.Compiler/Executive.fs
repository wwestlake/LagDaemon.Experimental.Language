namespace LagDaemon.Yadsl.Compiler


module Executive =
    open System.Text
    open System.IO
    open Model
    open FParsec

    /// Execute the successful result of parsing
    let exec (result:ParserResult<_,unit>) =
        ()

    /// Parse and execute a string
    let execString p (str:string) =
        let bytes = Encoding.ASCII.GetBytes(str)
        let stream = new MemoryStream( bytes, false )
        match runParserOnStream p UserState.Default "test" stream Encoding.UTF8 with
        | Success (result, _, _) -> 
            printfn "Success: %A" result
            //exec result
        | Failure (errorMessage, _, _) -> printfn "Failure: %s" errorMessage

    /// Parse and execute a file
    let execFile  (p:Parser<_>) (filename:string) =
        match runParserOnFile p UserState.Default filename Encoding.UTF8 with
        | Success (result, _, _) -> 
            printfn "Success: %A" result
            exec result
        | Failure (errorMessage, _, _) -> printfn "Failure: %s" errorMessage

    /// Parse and execute a stream
    let execStream  (p:Parser<_>) (name:string) (stream:Stream) =
        match runParserOnStream p UserState.Default name stream Encoding.UTF8 with
        | Success (result, _, _) -> 
            printfn "Success: %A" result
            exec result
        | Failure (errorMessage, _, _) -> printfn "Failure: %s" errorMessage

    

