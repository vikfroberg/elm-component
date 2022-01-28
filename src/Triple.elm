module Triple exposing
    ( Triple
    , mapFirst
    , mapSecond
    , mapThird
    )

type alias Triple a b c =
    ( a, b, c )

mapFirst f ( a, b, c ) =
    ( f a, b, c )

mapSecond f ( a, b, c ) =
    ( a, f b, c )

mapThird f ( a, b, c ) =
    ( a, b, f c )
