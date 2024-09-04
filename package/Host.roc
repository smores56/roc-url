module [Host, IpAddress]

import AsciiSet exposing [AsciiSet]

Host : [Domain Str, IpAddress IpAddress, OpaqueHost Str, EmptyHost]

## An IPv4 or an IPv6 address.
IpAddress : [Ipv4 U32, Ipv6 U128]

## A non-empty ASCII string that can be used for further processing.
OpaqueHost : Str

# publicSuffix : Host -> Result Str [NotADomain]
# publicSuffix = \host ->
#     when host is
#         IpAddress _addr | OpaqueHost _host | EmptyHost -> Err NotADomain
#         Domain domain ->
#             hasTrailingDot = Str.endsWith domain "."

# domainToAscii : { domain : Str, beStrict : Bool } -> Result

parseIpv4 = \address ->
    parts =
        address
        |> Str.split "."

    if List.last parts == Ok "" then
        Err TrailingPeriod
    else if List.len parts > 4 then
        Err TooManySegments
    else if List.len parts < 4 then
        Err TooFewSegments
    else
        parts
        |> List.mapWithIndex \part, index -> (part, index)
        |> List.mapTry \(part, index) ->
            Str.toU8 part
            |> Result.mapErr \InvalidNumStr -> InvalidU8Segment index

toStr = \host ->
    when host is
        Domain domain -> domainToStr domain
        IpAddress ipAddr ->
            when ipAddr is
                Ipv4 v4 -> ipv4ToStr v4
                Ipv6 v6 -> ipv6ToStr v6

        OpaqueHost opaque -> opaque
        EmptyHost -> ""

ipv4ToStr : U32 -> Str
ipv4ToStr = \ip ->
    getIpSegment = \index ->
        ip
        |> Num.shiftRightBy (32 - 8 * index)
        |> Num.toU8

    List.range { start: At 0, end: Before 4 }
    |> List.map \segment ->
        getIpSegment segment |> Num.toStr
    |> Str.joinWith ":"

ipv6ToStr : U128 -> Str
ipv6ToStr = \ip ->
    getIpSegment = \index ->
        ip
        |> Num.shiftRightBy (128 - 8 * index)
        |> Num.toU8
    # renderHex = \

    List.range { start: At 0, end: Before 8 }
    |> List.map \segment ->
        left = getIpSegment segment
        right = getIpSegment (segment + 1)

        "$(renderHex left)$(renderHex right)"
    |> Str.joinWith ":"

forbiddenHostCodePoints : AsciiSet
forbiddenHostCodePoints = AsciiSet.fromList [
    '\u(0)',
    '\t',
    '\n',
    '\r',
    ' ',
    '#',
    '/',
    ':',
    '<',
    '>',
    '?',
    '@',
    '[',
    '\\',
    ']',
    '^',
    '|',
]

forbiddenDomainCodePoints =
    AsciiSet.c0controlSet
    |> AsciiSet.combine forbiddenHostCodePoints
    |> AsciiSet.addAll ['%', 0x7f]

