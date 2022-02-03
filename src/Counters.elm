module Counters exposing (..)

import Element exposing (Element)
import Html
import List.Extra


type alias Model =
    List Int


type Msg
    = Increment
    | Decrement
    | CounterChanged Int Int


type alias Props =
    {}


init : Props -> (Msg -> msg) -> ( Model, Cmd msg )
init _ _ =
    ( [ 1 ]
    , Cmd.none
    )


propsChanged : Props -> Props -> (Msg -> msg) -> Model -> ( Model, Cmd msg )
propsChanged prevProps nextProps toSelf model =
    ( model, Cmd.none )


update : Props -> (Msg -> msg) -> Msg -> Model -> ( Model, Cmd msg )
update _ _ msg model =
    case msg of
        Increment ->
            ( model ++ [ 0 ]
            , Cmd.none
            )

        Decrement ->
            ( List.take (List.length model - 1) model
            , Cmd.none
            )

        CounterChanged id count ->
            ( List.Extra.setAt id count model
            , Cmd.none
            )


view :
    { components
        | button : { onClick : msg, label : String } -> Element (Html.Attribute msg) component
        , counter : { onChange : Int -> msg, startValue : Int } -> Element (Html.Attribute msg) component
    }
    -> (Msg -> msg)
    -> Model
    -> Element (Html.Attribute msg) component
view { button, counter } toSelf model =
    Element.div
        []
        [ button { onClick = toSelf Decrement, label = "Decrement" }
        , button { onClick = toSelf Increment, label = "Increment" }
        , Element.div
            []
            [ Element.text "Sum of all counters"
            , Element.text <| String.fromInt <| List.foldl (+) 0 model
            ]
        , model
            |> List.indexedMap (\i count -> counter { startValue = count, onChange = CounterChanged i >> toSelf })
            |> Element.div []
        ]
