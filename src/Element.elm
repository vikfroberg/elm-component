module Element exposing (..)

import Html exposing (Html)


type alias Attribute msg =
    Html.Attribute msg


type Element msg component
    = Text String
    | Node String (List (Html.Attribute msg)) (List (Element msg component))
    | Component component

text =
    Text

none =
    Text ""

div attributes children =
    Node "div" attributes children

button attributes children =
    Node "button" attributes children

component component_ =
    Component component_
