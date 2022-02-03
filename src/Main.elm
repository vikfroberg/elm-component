module Main exposing (..)

import Browser
import Html
import Element
import Component
import System

-- TODO: Generate boilerplate code
-- TODO: Add keyed elements
-- TODO: Add context to components like lang, translations, etc.
-- TODO: Example: Link component that fetches urls from route
-- TODO: Provide services to components so that it can communicate with services

componentConfig =
    { init = Component.init
    , update = Component.update
    , propsChanged = Component.propsChanged
    , view = Component.view
    , isEqual = Component.isEqual
    }


viewConfig =
    { text = Html.text
    , node = Html.node
    }


app =
    Element.component (Component.CountersProps {})


type alias Msg =
    System.Msg Component.Msg

type alias Model =
    System.Model (Html.Attribute Msg) (Component.Props Msg) Component.Model


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> System.init componentConfig app
        , update = System.update componentConfig app
        , view = System.view viewConfig
        , subscriptions = \_ -> Sub.none
        }
