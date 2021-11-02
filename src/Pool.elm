module Pool exposing (..)

import Dict exposing (Dict)


type Pool process
    = Pool Int (Dict Int process)


empty : Pool process
empty =
    Pool 0 Dict.empty

get : Int -> Pool process -> Maybe process
get id (Pool _ dict) =
    Dict.get id dict

insert : process -> Pool process -> ( Int, Pool process )
insert process (Pool currentId dict) =
    ( currentId
    , Pool (currentId + 1) (Dict.insert currentId process dict)
    )

remove : Int -> Pool process -> Pool process
remove id (Pool currentId dict) =
    Pool currentId (Dict.remove id dict)

