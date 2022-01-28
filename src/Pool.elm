module Pool exposing (..)

import Dict exposing (Dict)

type alias Id =
    Int

type Pool process
    = Pool Id (Dict Id process)


null : Id
null =
   0

empty : Pool process
empty =
    Pool 1 Dict.empty

get : Id -> Pool process -> Maybe process
get id (Pool _ dict) =
    Dict.get id dict

replace : Id -> process -> Pool process -> Pool process
replace id process (Pool currentId dict) =
    Pool currentId (Dict.insert id process dict)

alloc : Pool process -> ( Id, Pool process )
alloc (Pool currentId dict) =
    ( currentId
    , Pool (currentId + 1) dict
    )

insert : process -> Pool process -> ( Id, Pool process )
insert process (Pool currentId dict) =
    ( currentId
    , Pool (currentId + 1) (Dict.insert currentId process dict)
    )

remove : Id -> Pool process -> Pool process
remove id (Pool currentId dict) =
    Pool currentId (Dict.remove id dict)
