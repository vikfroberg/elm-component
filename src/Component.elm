module Component exposing (..)


type Component msg
    = Counters
    | Counter
    | Button { onClick : msg, label : String }


toComparable : Component msg -> String
toComparable component =
    case component of
        Button _ -> "button"
        Counter -> "counter"
        Counters -> "counters"
