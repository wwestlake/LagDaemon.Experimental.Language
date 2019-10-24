namespace LagDaemon.Yadsl.Compiler


module PrimParsers =
    open FParsec
    open Model

    let ws = spaces
    let ws1 = spaces1
    let str = pstring
    let strCI = pstringCI

    let pboolean : Parser<_> = str "true" <|> str "false" |>> (bool.Parse)

    let pfloatErr : Parser<_> = pfloat 

