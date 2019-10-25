namespace LagDaemon.Yadsl.Compiler


module IdentifierParsers =

    open Model
    open FParsec
    open PrimParsers
    open KeywordParser

    //let identStartChars = ['a'..'z'] @ ['A'..'Z'] @ ['_']
    //let identChars = identStartChars @ ['0'..'9']

    let pidentifier_g : Parser<_> =
        let isfirst c = isLetter c || c = '_'
        let isrest c = isLetter c || isDigit c || c = '_'
        many1Satisfy2L isfirst isrest "identifier" <?> (sprintf "The identifier '' contains invalid characters." )
    
    let pidentifier =
        pidentifier_g 
        >>= fun s -> 
                if isKeyword s 
                then fail (sprintf "The word '%s' is a keyword, it cannot be used as an identifier" s)
                else preturn (s |> TIdentifier)







