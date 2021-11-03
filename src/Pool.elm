module Pool exposing (..)

import Dict exposing (Dict)

type alias PID =
    Int

type Pool process
    = Pool PID (Dict PID process)


null : PID
null =
   0

empty : Pool process
empty =
    Pool 1 Dict.empty

get : PID -> Pool process -> Maybe process
get id (Pool _ dict) =
    Dict.get id dict

replace : PID -> process -> Pool process -> Pool process
replace id process (Pool currentId dict) =
    Pool currentId (Dict.insert id process dict)

insert : process -> Pool process -> ( PID, Pool process )
insert process (Pool currentId dict) =
    ( currentId
    , Pool (currentId + 1) (Dict.insert currentId process dict)
    )

remove : PID -> Pool process -> Pool process
remove id (Pool currentId dict) =
    Pool currentId (Dict.remove id dict)
