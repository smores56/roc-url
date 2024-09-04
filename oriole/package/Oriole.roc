module [
    match,
    map,
    oneOf,
    base,
    segment,
    any,
    u64,
    custom,
    RouteMatcher,
]

import Url

RouteMatcher data : Url.Url -> Result (data, Url.Url) [NotMatched]

match : RouteMatcher data, Url.Url -> Result data [NotMatched]
match = \matcher, url ->
    matcher url
    |> Result.try \(data, rest) ->
        if List.isEmpty rest.path then
            Ok data
        else
            Err NotMatched

map : RouteMatcher from, (from -> to) -> RouteMatcher to
map = \matcher, mapper ->
    \url ->
        matcher url
        |> Result.map \(data, rest) ->
            (mapper data, rest)

try : RouteMatcher from, (from -> Result to err) -> RouteMatcher to
try = \matcher, andThen ->
    \url ->
        matcher url
        |> Result.try \(data, rest) ->
            when andThen data is
                Ok success -> Ok (success, rest)
                Err _err -> Err NotMatched

combine : RouteMatcher a, RouteMatcher b, (a, b -> c) -> RouteMatcher c
combine = \firstMatcher, secondMatcher, combiner ->
    \url ->
        firstMatcher url
        |> Result.try \(first, afterFirst) ->
            secondMatcher afterFirst
            |> Result.try \(second, afterSecond) ->
                Ok (combiner first second, afterSecond)

oneOf : List (RouteMatcher data) -> RouteMatcher data
oneOf = \parsers ->
    \url ->
        List.walkUntil parsers (Err NotMatched) \state, parser ->
            when parser url is
                Ok (data, rest) -> Break (Ok (data, rest))
                Err NotMatched -> Continue state

base : RouteMatcher {}
base = \url ->
    if List.isEmpty url.path then
        Ok ({}, url)
    else
        Err NotMatched

segment : Str -> RouteMatcher {}
segment = \text ->
    \url ->
        when url.path is
            [first, .. as rest] if first == text ->
                Ok ({}, { url & path: rest })

            _ -> Err NotMatched

any : RouteMatcher Str
any = \url ->
    when url.path is
        [first, .. as rest] -> Ok (first, { url & path: rest })
        _ -> Err NotMatched

# TODO: handle all num types somehow
u64 : RouteMatcher U64
u64 = try any Str.toU64

custom : (Str -> Result data err) -> RouteMatcher data
custom = \parser ->
    try any parser

expect
    url = {
        segments: ["users", "123", "abc"],
        queryParams: Dict.empty {},
        fragment: "",
    }

    matcher =
        { combine <-
            _: segment "users",
            userId: u64,
            context: any,
        }

    matchResult = matcher |> match url

    matchResult
    == Ok { userId: 123, context: "abc" }

expect
    url = Url.parse "/user/123/abc"
    matcher =
        oneOf [
            segment "users" |> map Users,
            segment "posts" |> map Posts,
        ]

    matchResult = matcher |> match url

    matchResult
    == Ok (Users {})
