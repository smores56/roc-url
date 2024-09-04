interface Error
    exposes [
        IdnaError,
        HostParsingError,
        UrlParsingError,
        ValidationError,
    ]
    imports []

# Internationalized Domain Names in Applications (IDNA) errors
IdnaError : [
    DomainToAscii,
    DomainToUnicode,
]

HostParsingError : [
    DomainInvalidCodePoint,
    HostInvalidCodePoint,
    Ipv4EmptyPart,
    Ipv4TooManyParts,
    Ipv4NonNumericPart,
    Ipv4NonDecimalPart,
    Ipv4OutOfRangePart,
    Ipv6Unclosed,
    Ipv6InvalidCompression,
    Ipv6TooManyPieces,
    Ipv6MultipleCompression,
    Ipv6InvalidCodePoint,
    Ipv6TooFewPieces,
    Ipv4InIpv6TooManyPieces,
    Ipv4InIpv6InvalidCodePoint,
    Ipv4InIpv6OutOfRangePart,
    Ipv4InIpv6TooFewParts,
]

UrlParsingError : [
    InvalidUrlUnit,
    SpecialSchemeMissingFollowingSolidus,
    MissingSchemeNonRelativeUrl,
    InvalidReverseSolidus,
    InvalidCredentials,
    HostMissing,
    PortOutOfRange,
    PortInvalid,
    FileInvalidWindowsDriveLetter,
    FileInvalidWindowsDriveLetterHost,
]

ValidationError : [
    Idna IdnaError,
    HostParsing HostParsingError,
    UrlParsing UrlParsingError,
]
