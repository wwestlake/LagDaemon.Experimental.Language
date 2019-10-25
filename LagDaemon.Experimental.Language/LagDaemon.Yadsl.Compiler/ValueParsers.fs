namespace LagDaemon.Yadsl.Compiler


module ValueParsers =

    open Model
    open FParsec
    open PrimParsers

    /// converts a hex character to it integer value
    let hex2int c = (int c &&& 15) + (int c >>> 6) * 9 |> int64

    let power b exp =
        let rec inner e acc =
            if e = 0 then acc else inner (e - 1) (acc * b)
        inner exp 1 |> int64


    /// Parses a series of hex characters
    let hexStr : Parser<_> = many hex

    /// Converts a list of hex digits to an integer
    let hexListToInt list =
        let rec inner rest acc pos =
            match rest with
            | [] -> acc
            | h::t -> inner t (acc + (hex2int h) * (power 16 pos)) (pos - 1)
        inner list (int64 0) (List.length list - 1)

    let escape : Parser<_> = 
        anyOf "\"\\/bnfrt"
            |>> function
                | 'b' -> char "\b"
                | 'f' -> char "\u000C"
                | 'n' -> char "\n"
                | 'r' -> char "\r"
                | 't' -> char "\t"
                | c -> c

    let unicodeEscape : Parser<_> =
        str "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
            (hex2int h3) * (int64 4096) + (hex2int h2) * (int64 256) + (hex2int h1) * (int64 16) + hex2int h0
            |> char
        )



    /// parses a string literal
    let stringLiteral : Parser<_> =
        let escapeStr = escape |>> string
        let unicodeEscapeStr = unicodeEscape |>> string
        let escapeCharSnippet = str "\\" >>. ( escapeStr <|> unicodeEscapeStr)
        let normalCharSnippet = manySatisfy (fun c -> c <> '"' && c <> '\\')

        between (str "\"") (str "\"") (stringsSepBy normalCharSnippet escapeCharSnippet) |>> TString
           
           
    /// parses a hex literal
    let hexLiteral : Parser<_> =
        str "0x" >>. hexStr |>> hexListToInt |>> TInteger


    let charLiteral = between (pchar '\'') (pchar '\'') 
                        (
                            escape <|> 
                            unicodeEscape <|> anyChar
                        ) |>> TChar



    let boolLiteral : Parser<_> = pboolean |>> TBool
    let intLiteral : Parser<_> = pint64 |>> TInteger
    let floatLiteral : Parser<_> = pfloat |>> TFloat
    let longLiteral : Parser<_> = puint64 .>> pchar 'u' |>> TLong    
    let byteLiteral : Parser<_> = pint8 .>> pchar 'b' |>> byte |>> TByte

    let floatOrIntegerLiteral : Parser<_> = 
        pnumber |>> (fun x -> match x with 
                              | Int i -> TInteger i
                              | Float f -> TFloat f
                    )

    let literalValue : Parser<_> =
              [
                  stringLiteral
                  hexLiteral
                  charLiteral
                  boolLiteral
                  longLiteral
                  byteLiteral
                  floatOrIntegerLiteral 
              ] |> List.map attempt |> choice

