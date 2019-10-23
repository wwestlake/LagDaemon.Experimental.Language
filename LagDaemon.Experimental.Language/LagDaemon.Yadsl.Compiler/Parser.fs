namespace LagDaemon.Yadsl.Compiler


module Parser =
    open KeywordParser

    let test (p:Parser<_>) (str:string) =
        match run p str with
        | Success (result, _, _) -> printfn "Success: %A" result
        | Failure (errorMessage, _, _) -> printfn "Failure: %s" errorMessage
