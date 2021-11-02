module Main exposing (main)

import Html
import Browser
import Dict exposing (Dict)

main : Program () Bootstrap Msg
main =
    Browser.element
        { init = \_ -> init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init =
    let
        model =
            { processes = Dict.empty
            , lastProcessId = 0
            }
    in
    ( model
    , Cmd.none
    )

update msg model =
    ( model
    , Cmd.none
    )

view model =
    Html.text ""


type alias PID =
    Int


type alias Bootstrap =
    { processes : Dict Int Model
    , lastProcessId : Int
    }



---


type DomAttribute msg
    = OnClick msg

type Model
    = ButtonModel ButtonModel
    | AppModel AppModel

type Msg
    = ButtonMsg (ButtonMsg Msg)
    | AppMsg AppMsg
    | NoOp

type Component msg
    = Root (Component msg)
    | Dom String (List (DomAttribute msg)) (List (Component msg))
    | Text String
    | Button msg String


---

type alias AppProps =
    {}

type alias AppModel =
    ()

type AppMsg
    = AppButtonClicked
    | AppPropChanged AppProps

app =
    { init = appInit
    , update = appUpdate
    , view = appView
    , onPropChange = AppPropChanged
    }

appInit props =
    ( ()
    , Cmd.none
    )

appUpdate msg props model =
    case msg of
        AppPropChanged _ ->
            ( (), Cmd.none )

        AppButtonClicked ->
            ( (), Cmd.none )

appView props model =
    Dom "div"
        []
        [ Dom "h1" [] [ Text "Example app" ]
        , Button AppButtonClicked "Click me"
        ]


---

type alias ButtonProps msg =
    { onChange : msg
    , label : String
    }

type alias ButtonModel =
    ()

type ButtonMsg msg
    = ButtonPropChanged (ButtonProps msg)

button =
    { init = buttonInit
    , update = buttonUpdate
    , view = buttonView
    , onPropChange = ButtonPropChanged
    }


buttonInit props =
    ( (), Cmd.none)

buttonUpdate msg props model =
    ( (), Cmd.none )

buttonView props model =
    Dom "button"
        [ OnClick props.onClick
        ]
        [ Text props.label
        ]
