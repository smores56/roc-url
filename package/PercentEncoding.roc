module [
    encode,
    encodeUtf8Bytes,
    decode,
    decodeUtf8Bytes,
]

import AsciiSet exposing [AsciiSet]

encode : Str -> Str
encode = \s ->
    encodeUtf8Bytes { bytes: Str.toUtf8 s, encodeSet: AsciiSet.userInfoSet }
    |> Str.fromUtf8
    |> Result.withDefault ""

encodeUtf8Bytes : { bytes : List U8, encodeSet : AsciiSet, spaceAsPlus ? Bool } -> List U8
encodeUtf8Bytes = \{ bytes, encodeSet, spaceAsPlus ? Bool.false } ->
    List.joinMap bytes \byte ->
        if spaceAsPlus && byte == ' ' then
            ['+']
        else if AsciiSet.contains encodeSet byte then
            encodeByte byte
        else
            [byte]

encodeByte : U8 -> List U8
encodeByte = \byte ->
    toUpperHex = \val ->
        if val < 10 then
            '0' + val
        else
            'A' + (val - 10)

    ['%', toUpperHex (Num.shiftRightBy byte 4), toUpperHex byte]

decode : Str -> Result Str [BadUtf8]
decode = \text ->
    text
    |> Str.toUtf8
    |> decodeUtf8Bytes
    |> Str.fromUtf8
    |> Result.mapErr \_err -> BadUtf8

decodeUtf8Bytes : List U8 -> List U8
decodeUtf8Bytes = \bytes ->
    fromHex = \b ->
        if b >= '0' && b <= '9' then
            Ok (b - '0')
        else if b >= 'a' && b <= 'f' then
            Ok (b + 10 - 'a')
        else if b >= 'A' && b <= 'F' then
            Ok (b + 10 - 'A')
        else
            Err InvalidHex

    stateAfter =
        List.walk bytes { action: GetNext, collected: [] } \{ action, collected }, byte ->
            when action is
                GetNext ->
                    if byte == '%' then
                        { action: CheckFirstByte, collected }
                    else
                        { action: GetNext, collected: collected |> List.append byte }

                CheckFirstByte ->
                    when fromHex byte is
                        Ok firstHex ->
                            { action: CheckSecondByte { first: byte, firstHex }, collected }

                        Err _err ->
                            { action: GetNext, collected: collected |> List.concat ['%', byte] }

                CheckSecondByte { first, firstHex } ->
                    when fromHex byte is
                        Ok secondHex ->
                            decoded = (Num.shiftLeftBy firstHex 4) + secondHex
                            { action: GetNext, collected: collected |> List.append decoded }

                        Err _err ->
                            { action: GetNext, collected: collected |> List.concat ['%', first, byte] }

    when stateAfter is
        { action: GetNext, collected } ->
            collected

        { action: CheckFirstByte, collected } ->
            collected |> List.append '%'

        { action: CheckSecondByte { first, firstHex: _ }, collected } ->
            collected |> List.concat ['%', first]

expect encode "#" == "%23"
expect encode "\u(127)" == "%7F"

# Percent-decode input  `%25%s%1G`  `%%s%1G`
# Percent-decode input  "‽%25%2E"  0xE2 0x80 0xBD 0x25 0x2E
# Percent-encode after encoding with Shift_JIS, input, and the userinfo percent-encode set  " "  "%20"
# "≡"  "%81%DF"
# "‽"  "%26%238253%3B"
# Percent-encode after encoding with ISO-2022-JP, input, and the userinfo percent-encode set  "¥"  "%1B(J\%1B(B"
# Percent-encode after encoding with Shift_JIS, input, the userinfo percent-encode set, and true  "1+1 ≡ 2%20‽"  "1+1+%81%DF+2%20%26%238253%3B"
# UTF-8 percent-encode input using the userinfo percent-encode set  U+2261 (≡)  "%E2%89%A1"
# U+203D (‽)  "%E2%80%BD"
# UTF-8 percent-encode input using the userinfo percent-encode set  "Say what‽"  "Say%20what%E2%80%BD"
