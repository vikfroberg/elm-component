module Return exposing
    ( Return
    , do
    , andThen
    , map
    , andMap
    , return
    , addCmd
    )


type alias Return model msg =
    ( model, Cmd msg )


do : Return a msg -> (a -> Return b msg) -> Return b msg
do a f =
    andThen f a


addCmd : (a -> Cmd msg) -> Return a msg -> Return a msg
addCmd toCmd =
    andThen (\m -> ( m, toCmd m ))


andThen : (a -> Return b msg) -> Return a msg -> Return b msg
andThen f ( m1, c1 ) =
    let ( m2, c2 ) = f m1 in
        ( m2, Cmd.batch [ c1, c2 ] )

map : (a -> b) -> Return a msg -> Return b msg
map f ( m, c ) =
    ( f m, c )

andMap : Return a msg -> Return (a -> b) msg -> Return b msg
andMap ( m, c1 ) ( f, c2 ) =
    ( f m, Cmd.batch [ c1, c2 ] )

return : a -> Return a msg
return m =
    ( m, Cmd.none )
