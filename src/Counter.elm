module Counter exposing (..)

import Element exposing (Element)


type alias Model =
    Int


type Msg
    = Increment
    | Decrement


init : Model
init =
    0


-- TODO: Call all processes with the global store when changed?
-- No need to keep track of sending messages back.
-- dataChanged : globalDataStore -> ( Model, Cmd msg )
--     case Dict.get model.id globalDataStore of
--         Just fanns -> { model | data = fanns }
--         Nothing -> ({ model | data = laddar }, Cmd.send fråga efterData )


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1


view :
    { components
        | button : { onClick : msg, label : String } -> Element msg component
    }
    -> (Msg -> msg)
    -> Model
    -> Element msg component
view { button } toSelf model =
    Element.div
        []
        [ button { onClick = toSelf Decrement, label = "Decrement" }
        , Element.text (String.fromInt model)
        , button { onClick = toSelf Increment, label = "Increment" }
        ]
