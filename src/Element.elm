module Element exposing (..)


type Element attribute component
    = Text String
    | Node String (List attribute) (List (Element attribute component))
    | Component component


text : String -> Element attribute component
text =
    Text


none : Element attribute component
none =
    Text ""


div : List attribute -> List (Element attribute component) -> Element attribute component
div attributes children =
    Node "div" attributes children


button : List attribute -> List (Element attribute component) -> Element attribute component
button attributes children =
    Node "button" attributes children


component : component -> Element attribute component
component a =
    Component a
