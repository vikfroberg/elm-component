port module Mock exposing (..)

import Browser
import Element exposing (Element)
import Component exposing (Component)
import Pool exposing (Pool)
import Html exposing (Html)
import Counter
import Counters
import Button

app =
    Element.component Component.Counters


type alias Model =
    ( Tree String
    , Pool ProcessModel
    )

type ProcessModel
    = CounterModel Counter.Model
    | CountersModel Counters.Model

type Tree comparable
    = ComponentWithProcess Int comparable (Tree comparable)
    | Component comparable (Tree comparable)
    | Node comparable (List (Tree comparable))
    | Text

type Msg
    = SendToProcess Int ProcessMsg
    | NoOp

type ProcessMsg
    = CounterMsg Counter.Msg
    | CountersMsg Counters.Msg


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


components =
    { counters = Element.Component Component.Counters
    , counter = Element.Component Component.Counter
    , button = \props -> Element.Component (Component.Button props)
    }


init : Model
init =
    let
        go : Element Msg (Component Msg) -> Pool ProcessModel -> Model
        go element processPool =
            case element of
                Element.Component component ->
                    case component of
                        Component.Button props ->
                            let
                                ( el, processPool_ ) =
                                    go (Button.view components props) processPool
                            in
                            ( Component (Component.toComparable component) el
                            , processPool_
                            )

                        Component.Counter ->
                            let
                                model =
                                    Counter.init

                                ( pid, processPool_ ) =
                                    Pool.insert (CounterModel model) processPool

                                ( el, processPool__ ) =
                                    go (Counter.view components (always NoOp) model) processPool_
                            in
                            ( ComponentWithProcess pid (Component.toComparable component) el
                            , processPool__
                            )

                        Component.Counters ->
                            let
                                model =
                                    Counters.init

                                ( pid, processPool_ ) =
                                    Pool.insert (CountersModel model) processPool

                                ( el, processPool__ ) =
                                    go (Counters.view components (always NoOp) model) processPool_
                            in
                            ( ComponentWithProcess pid (Component.toComparable component) el
                            , processPool__
                            )

                Element.Node t _ children ->
                    let
                        ( processPool_, children_ ) =
                            List.foldl
                                (\el ( p, kids ) ->
                                    let ( kid, p_ ) = go el p in ( p_, kids ++ [ kid ] )
                                )
                                ( processPool, [] )
                                children
                    in
                    ( Node t children_
                    , processPool_
                    )

                Element.Text _ ->
                    ( Text, processPool )
    in
    go app Pool.empty


view : Model -> Html Msg
view ( rootTree, processPool ) =
    let
        go : Element Msg (Component Msg) -> Tree String -> Html Msg
        go element tree =
            case element of
                Element.Component component ->
                    case component of
                        Component.Button props ->
                            case tree of
                                Component _ subTree ->
                                    go (Button.view components props) subTree

                                _ ->
                                    Html.text ""

                        Component.Counter ->
                            case tree of
                                ComponentWithProcess pid _ subTree ->
                                    case Pool.get pid processPool of
                                        Just (CounterModel model) ->
                                            go (Counter.view components (always NoOp) model) subTree

                                        _ ->
                                            Html.text ""

                                _ ->
                                    Html.text ""

                        Component.Counters ->
                            case tree of
                                ComponentWithProcess pid _ subTree ->
                                    case Pool.get pid processPool of
                                        Just (CountersModel model) ->
                                            go (Counters.view components (always NoOp) model) subTree

                                        _ ->
                                            Html.text ""

                                _ ->
                                    Html.text ""

                Element.Node t attributes children ->
                    case tree of
                        Node _ kids ->
                            Html.node t attributes (List.map2 go children kids)

                        _ ->
                            Html.text ""

                Element.Text s ->
                    Html.text s
    in
    go app rootTree
    -- Html.pre [] [ Html.text <| Debug.toString ( rootTree, processPool ) ]
    -- let
    --     go element tree =
    --         case ( element, tree ) of
    --             ( Element.Node attributes children, Node

    -- in
    -- go app rootTree


update msg model =
    model


-- type alias Component props model msg effect view =
--     { init : props -> ( model, effect )
--     , update : props -> msg -> model -> ( model, effect )
--     , view : props -> model -> Element
--     }

-- type alias Page route model msg effect view meta =
--     { init : route -> ( model, List effect )
--     , update : props -> msg -> model -> ( model, List effect )
--     , view : props -> model -> Tree view
--     , meta : props -> model -> meta
--     }

-- type alias Service config model msg effect =
--     { init : config -> ( model, List effect )
--     , update : msg -> model -> ( model, List effect )
--     }

-- type alias PID = Int
-- type alias MID = Int

port send : { payload : String, replyTo : Int } -> Cmd msg
port receive : ({ payload : String, replyTo : Int } -> msg) -> Sub msg

-- type alias Model process msg =
--     { process : Dict PID process -- TODO: Kill kids when parent is removed
--     , lastPid : PID -- Make into a data type together with Dict
--     , msg : Dict MID (String -> Msg msg) -- TODO: Remove when process is killed
--     , lastMid : MID
--     }

-- type Msg msg
--     = SendTo PID msg
--     | MsgDecodeError MID String
--     | Kill PID

-- -- TODO: Add debugging to track ports and messages

-- -- TODO: En enkel diffing argo kommer man ganska långt med, eftersom vi bara ska spawna för komponenter. Det kommer inte vara lika många för andra.
-- -- Men vi måste behålla förra så att vi kan diffa.

-- -- TODO: This is transformed into a tree of union type
-- { div : List (Html.Attribute msg)
-- , k
