module Counters exposing (..)

import Element exposing (Element)


type alias Model =
    Int


type Msg
    = Increment
    | Decrement


init : Model
init =
    1

--propsChanged : Props -> Model -> ( Model, Cmd msg )
--propsChanged =
--    --


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
        , counter : Element msg component
    }
    -> (Msg -> msg)
    -> Model
    -> Element msg component
view { button, counter } toSelf model =
    Element.div
        []
        [ button { onClick = toSelf Decrement, label = "Decrement" }
        , button { onClick = toSelf Increment, label = "Increment" }
        , Element.div
            []
            (List.repeat model counter)
        ]
