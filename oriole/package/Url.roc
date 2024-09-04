module [parse, toStr, Url]

Url : {
    protocol : Str,
    host : Str,
    port : Result U16 [NoPort],
    path : List Str,
    query : Result (Dict Str Str) [NoQuery],
    fragment : Result Str [NoFragment],
}

parse : Str -> Result Url [InvalidUrl]
parse = \_text ->
    Err InvalidUrl

toStr : Url -> Str
toStr = \_url ->
    ""
