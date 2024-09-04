module [
    AsciiSet,
    empty,
    add,
    addAll,
    combine,
    fromList,
    remove,
    contains,
    c0controlSet,
    fragmentSet,
    querySet,
    specialQuerySet,
    pathSet,
    userInfoSet,
    componentSet,
    applicationFormUrlEncodedSet,
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

combine : AsciiSet, AsciiSet -> AsciiSet
combine = \@AsciiSet set1, @AsciiSet set2 ->
    @AsciiSet {
        high: Num.bitwiseOr set1.high set2.high,
        low: Num.bitwiseOr set1.low set2.low,
    }

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

c0controlSet =
    List.range { start: At 0x00, end: At 0x1F }
    |> List.append 0x7F
    |> fromList

expect contains c0controlSet 0x00
expect contains c0controlSet 0x1F
expect !(contains c0controlSet 0x20)
expect !(contains c0controlSet 0x7E)
expect contains c0controlSet 0x7F

fragmentSet =
    c0controlSet
    |> addAll [' ', '"', '<', '>', '`']

querySet =
    c0controlSet
    |> addAll [' ', '"', '#', '<', '>']

specialQuerySet =
    querySet |> add '\''

pathSet =
    querySet
    |> addAll ['?', '`', '{', '}']

userInfoSet =
    pathSet
    |> addAll ['/', ':', ';', '=', '@', '|']
    |> addAll (List.range { start: At '[', end: At '^' })

componentSet =
    userInfoSet
    |> addAll ['$', '&', '+', ',']

applicationFormUrlEncodedSet =
    componentSet
    |> addAll ['!', '~']
    |> addAll (List.range { start: At '\'', end: At ']' })

# nonAlphanumeric =
#     c0controlSet
#     |> addAll [
#         ' ',
#         '!',
#         '"',
#         '#',
#         '$',
#         '%',
#         '&',
#         '\'',
#         '(',
#         ')',
#         '*',
#         '+',
#         ',',
#         '-',
#         '.',
#         '/',
#         ':',
#         ';',
#         '<',
#         '=',
#         '>',
#         '?',
#         '@',
#         '[',
#         '\\',
#         ']',
#         '^',
#         '_',
#         '`',
#         '{',
#         '|',
#         '}',
#         '~',
#     ]
