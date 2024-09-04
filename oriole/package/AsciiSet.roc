module [
    empty,
    add,
    addAll,
    fromList,
    remove,
    contains,
    controls,
    nonAlphanumeric,
    AsciiSet,
]

AsciiSet := {
    high : U128,
    low : U128,
}

empty : AsciiSet
empty = @AsciiSet { high: 0, low: 0 }

add : AsciiSet, U8 -> AsciiSet
add = \@AsciiSet { high, low }, char ->
    if char < 128 then
        lowWithChar =
            Num.shiftLeftBy 1 char
            |> Num.bitwiseOr low

        @AsciiSet { high, low: lowWithChar }
    else
        highWithChar =
            Num.shiftLeftBy 1 (char - 128)
            |> Num.bitwiseOr high

        @AsciiSet { high: highWithChar, low }

addAll : AsciiSet, List U8 -> AsciiSet
addAll = \set, chars ->
    List.walk chars set add

fromList : List U8 -> AsciiSet
fromList = \chars ->
    List.walk chars empty add

remove : AsciiSet, U8 -> AsciiSet
remove = \@AsciiSet { high, low }, char ->
    if char < 128 then
        lowWithoutChar =
            Num.shiftLeftBy 1 char
            |> Num.bitwiseNot
            |> Num.bitwiseAnd low

        @AsciiSet { high, low: lowWithoutChar }
    else
        highWithoutChar =
            Num.shiftLeftBy 1 (char - 128)
            |> Num.bitwiseNot
            |> Num.bitwiseAnd high

        @AsciiSet { high: highWithoutChar, low }

contains : AsciiSet, U8 -> Bool
contains = \@AsciiSet { high, low }, char ->
    if char < 128 then
        mask = Num.shiftLeftBy 1 char
        (Num.bitwiseAnd low mask) != 0
    else
        mask = Num.shiftLeftBy 1 (char - 128)
        (Num.bitwiseAnd high mask) != 0

controls =
    List.range { start: At 0x00, end: At 0x1F }
    |> List.append 0x7F
    |> fromList

expect contains controls 0x00
expect contains controls 0x1F
expect !(contains controls 0x20)
expect !(contains controls 0x7E)
expect contains controls 0x7F

nonAlphanumeric =
    controls
    |> addAll [
        ' ',
        '!',
        '"',
        '#',
        '$',
        '%',
        '&',
        '\'',
        '(',
        ')',
        '*',
        '+',
        ',',
        '-',
        '.',
        '/',
        ':',
        ';',
        '<',
        '=',
        '>',
        '?',
        '@',
        '[',
        '\\',
        ']',
        '^',
        '_',
        '`',
        '{',
        '|',
        '}',
        '~',
    ]
