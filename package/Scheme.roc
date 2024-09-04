module [defaultSchemePorts]

defaultSchemePorts =
    Dict.fromList [
        ("ftp", 21),
        ("http", 80),
        ("https", 443),
        ("ws", 80),
        ("wss", 443),
    ]

specialSchemes =
    Set.fromList [
        "ftp",
        "file",
        "http",
        "https",
        "ws",
        "wss",
    ]
