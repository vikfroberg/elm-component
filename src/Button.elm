module Button exposing (..)

import Element exposing (Element)
import Element.Events


type alias Props msg =
    { onClick : msg
    , label : String
    }


view :
    components
    -> Props msg
    -> Element msg component
view _ { onClick, label } =
    Element.button [ Element.Events.onClick onClick ] [ Element.text label ]
