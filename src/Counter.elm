module Counter exposing (..)

import Element exposing (Element)
import Cmd


type alias Model =
    Int


type Msg
    = Increment
    | Decrement


type alias Props msg =
    { onChange : Int -> msg
    , startValue : Int
    }


init : Props msg -> (Msg -> msg) -> ( Model, Cmd msg )
init { startValue } toSelf =
    ( startValue
    , Cmd.none
    )


propsChanged : Props msg -> Props msg -> (Msg -> msg) -> Model -> ( Model, Cmd msg )
propsChanged prevProps nextProps toSelf model =
    ( model, Cmd.none )


update : Props msg -> (Msg -> msg) -> Msg -> Model -> ( Model, Cmd msg )
update { onChange } toSelf msg model =
    case msg of
        Increment ->
            ( model + 1
            , Cmd.send <| onChange (model + 1)
            )

        Decrement ->
            ( model - 1
            , Cmd.send <| onChange (model - 1)
            )


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
        , button { onClick = toSelf Increment, label = "Increment " ++ String.fromInt model }
        ]
