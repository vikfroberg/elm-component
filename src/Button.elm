module Button exposing (..)

import Element exposing (Element)
import Element.Events
import Cmd


component =
    { init = init
    , update = update
    , view = view
    }


type alias Props msg =
    { onClick : msg
    , label : String
    }


type alias Model =
    { label : String
    }


type Msg
    = OnClick


init : Props msg -> (Msg -> msg) -> ( Model, Cmd msg )
init { label } _ =
    ( { label = label }
    , Cmd.none
    )


propsChanged : Props msg -> Props msg -> (Msg -> msg) -> Model -> ( Model, Cmd msg )
propsChanged prevProps nextProps toSelf model =
    ( { label = nextProps.label }, Cmd.none )


update : Props msg -> (Msg -> msg) -> Msg -> Model -> ( Model, Cmd msg )
update { onClick } _ msg model =
    case msg of
        OnClick ->
            ( model
            , Cmd.send onClick
            )


view :
    components
    -> (Msg -> msg)
    -> Model
    -> Element msg component
view _ toSelf { label } =
    Element.button
        [ Element.Events.onClick <| toSelf OnClick ]
        [ Element.text label ]
