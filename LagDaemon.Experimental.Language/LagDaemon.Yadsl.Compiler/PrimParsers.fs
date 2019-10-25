namespace LagDaemon.Yadsl.Compiler


module PrimParsers =
    open FParsec
    open Model

    let ign x = charsTillString x false System.Int32.MaxValue

    let pboolean : Parser<_> = pstring "true" <|> pstring "false" |>> (bool.Parse)

    let stringToFloat (str:string) = str |> float


    type Number = Int of int64
                  | Float of float

            // -?[0-9]+(\.[0-9]*)?([eE][+-]?[0-9]+)?
    let numberFormat =  NumberLiteralOptions.AllowMinusSign
                        ||| NumberLiteralOptions.AllowFraction
                        ||| NumberLiteralOptions.AllowExponent
    
    let pnumber : Parser<Number> =
        numberLiteral numberFormat "number"
        |>> fun nl -> if nl.IsInteger then Int (int64 nl.String)
                      else Float (float nl.String)

    let maxCount = System.Int32.MaxValue
    let lineComment : Parser<_> =
        pstring "//" >>. many1Satisfy ((<>) '\n')
    


    let openMultiLineComment = "(*"
    let closeMultiLineComment = "*)"
    
    let blockCommentReply o =
        let rec inner o =   
            between (pstring openMultiLineComment) 
                    (pstring closeMultiLineComment) 
                    (attempt (ign openMultiLineComment >>. inner >>. ign closeMultiLineComment) 
                    <|> ign closeMultiLineComment) <| o
        inner o
    
    let blockComment : Parser<_> = blockCommentReply

    let pcomment : Parser<_> =
        [
            lineComment
            blockComment
        ] |> choice
       
    let pspaces = spaces >>. many (spaces >>. pcomment >>. spaces)   
    let ws : Parser<_> = pspaces >>. many (pspaces >>. blockComment >>. pspaces) |>> (fun _ -> ())
    let ws1 = spaces1
    let str = pstring
    let str_ws s = pstring s .>> ws
    let str_ws1 s = pstring s .>> ws
    let strCI = pstringCI


