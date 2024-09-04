module [Url]

import Host exposing [Host]

Url := {
    scheme : Str,
    username : Str,
    password : Str,
    host : Result Host [NoHost],
    port : Result U16 [NoPort],
    path : List Str,
    query : Result Str [NoQuery],
    fragment : Result Str [NoFragment],
    blob : Result (List U8) [NoBlob],
}
