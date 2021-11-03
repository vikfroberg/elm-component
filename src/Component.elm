module Component exposing (..)


type Component msg
    = Counters
    | Counter
    | Button { onClick : msg, label : String }


toKey : Component msg -> String
toKey component =
    case component of
        Button _ -> "button"
        Counter -> "counter"
        Counters -> "counters"
