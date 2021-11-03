module Main exposing (..)

import Browser
import Element exposing (Element)
import Component exposing (Component)
import Pool exposing (Pool)
import Html exposing (Html)
import Counter
import Counters
import Button
import List.Extra


-- TODO: Add keyed nodes and components

-- TODO: Add debugging to track ports and messages
-- port send : { payload : String, replyTo : Int } -> Cmd msg
-- port receive : ({ payload : String, replyTo : Int } -> msg) -> Sub msg


app =
    Element.component Component.Counters


type alias Model =
    { prevTree : Tree
    , processes : Pool ProcessModel
    }

type ProcessModel
    = CounterModel Counter.Model
    | CountersModel Counters.Model

type Tree
    = Component Int String Tree
    | Node String (List Tree)
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
    , submitButton = \onClick -> Element.Component (Component.Button { onClick = onClick, label = "Submit" })
    }


initElement : Element Msg (Component Msg) -> Pool ProcessModel -> ( Tree, Pool ProcessModel )
initElement element processPool =
    case element of
        Element.Component component ->
            case component of
                Component.Button props ->
                    let
                        ( el, processPool_ ) =
                            initElement (Button.view components props) processPool
                    in
                    ( Component Pool.null (Component.toKey component) el
                    , processPool_
                    )

                Component.Counter ->
                    let
                        model =
                            Counter.init

                        ( pid, processPool_ ) =
                            Pool.insert (CounterModel model) processPool

                        ( el, processPool__ ) =
                            initElement (Counter.view components (CounterMsg >> SendToProcess pid) model) processPool_
                    in
                    ( Component pid (Component.toKey component) el
                    , processPool__
                    )

                Component.Counters ->
                    let
                        model =
                            Counters.init

                        ( pid, processPool_ ) =
                            Pool.insert (CountersModel model) processPool

                        ( el, processPool__ ) =
                            initElement (Counters.view components (CountersMsg >> SendToProcess pid) model) processPool_
                    in
                    ( Component pid (Component.toKey component) el
                    , processPool__
                    )

        Element.Node t _ children ->
            let
                ( children_, processPool_ ) =
                    initChildren children processPool
            in
            ( Node t children_
            , processPool_
            )

        Element.Text _ ->
            ( Text, processPool )


initChildren children processPool =
    List.foldl
        (\el ( kids, p ) ->
            let
                ( kid, p_ ) = initElement el p
            in
            ( kids ++ [ kid ], p_ )
        )
        ( [], processPool )
        children


init : Model
init =
    let
        ( prevTree, processes ) =
            initElement app Pool.empty
    in
    { prevTree = prevTree
    , processes = processes
    }


view : Model -> Html Msg
view { prevTree, processes } =
    let
        go : Element Msg (Component Msg) -> Tree -> Html Msg
        go element tree =
            case ( element, tree ) of
                ( Element.Component component, Component pid key subTree ) ->
                    case component of
                        Component.Button props ->
                            go (Button.view components props) subTree

                        Component.Counter ->
                            case Pool.get pid processes of
                                Just (CounterModel model) ->
                                    go (Counter.view components (CounterMsg >> SendToProcess pid) model) subTree

                                _ ->
                                    Html.text ""

                        Component.Counters ->
                            case Pool.get pid processes of
                                Just (CountersModel model) ->
                                    go (Counters.view components (CountersMsg >> SendToProcess pid) model) subTree

                                _ ->
                                    Html.text ""

                ( Element.Node t attributes children, Node key subTrees ) ->
                    Html.node t attributes (List.map2 go children subTrees)

                ( Element.Text s, Text ) ->
                    Html.text s

                _ ->
                    Html.text ""
    in
    go app prevTree


-- TODO: En enkel diffing algo kommer man ganska långt med, eftersom vi bara ska spawna för komponenter.
-- Det kommer inte vara lika många element som i dom diffing.
update msg model =
    case msg of
        SendToProcess pid processMsg ->
            case ( processMsg, Pool.get pid model.processes ) of
                ( CounterMsg subMsg, Just (CounterModel subModel) ) ->
                    { model
                        | processes = Pool.replace pid (CounterModel (Counter.update subMsg subModel)) model.processes
                    }
                    |> diff

                ( CountersMsg subMsg, Just (CountersModel subModel) ) ->
                    { model
                        | processes = Pool.replace pid (CountersModel (Counters.update subMsg subModel)) model.processes
                    }
                    |> diff

                _ ->
                    model

        NoOp ->
            model


diffChildren : List (Element Msg (Component Msg)) -> List Tree -> Pool ProcessModel -> ( List Tree, Pool ProcessModel )
diffChildren elements trees processPool =
    if List.length elements > List.length trees then
        let
            ( children, processPool_ ) =
                diffChildren (List.take (List.length trees) elements) trees processPool

            ( restChildren, processPool__ ) =
                initChildren (List.drop (List.length trees) elements) processPool_
        in
        ( children ++ restChildren, processPool__ )
    else if List.length elements < List.length trees then
        let
            ( children, processPool_ ) =
                diffChildren elements (List.take (List.length elements) trees) processPool
        in
        ( children, killTrees (List.drop (List.length elements) trees) processPool_ )
    else
        List.foldl
            (\( e, t ) ( kids, p ) ->
                let
                    ( e_, p_ ) = diffElement e t p
                in
                ( kids ++ [ e_ ], p_ )
            )
            ( [], processPool )
            (List.Extra.zip elements trees)


diffElement : Element Msg (Component Msg) -> Tree -> Pool ProcessModel -> ( Tree, Pool ProcessModel )
diffElement element tree processPool =
    case ( element, tree ) of
        ( Element.Component component, Component pid key subTree ) ->
            if Component.toKey component == key then
                case component of
                    Component.Button props ->
                        diffElement (Button.view components props) subTree processPool
                            |> Tuple.mapFirst (Component pid key)

                    Component.Counter ->
                        case Pool.get pid processPool of
                            Just (CounterModel model) ->
                                diffElement (Counter.view components (CounterMsg >> SendToProcess pid) model) subTree processPool
                                    |> Tuple.mapFirst (Component pid key)

                            _ ->
                                Debug.todo "should never happen"

                    Component.Counters ->
                        case Pool.get pid processPool of
                            Just (CountersModel model) ->
                                diffElement (Counters.view components (CountersMsg >> SendToProcess pid) model) subTree processPool
                                    |> Tuple.mapFirst (Component pid key)

                            _ ->
                                Debug.todo "should never happen"
            else
                initElement element (killTree tree processPool)

        ( Element.Node key attributes children, Node key_ subTrees ) ->
            if key == key_ then
                let
                    ( subTrees_, processPool_ ) =
                        diffChildren children subTrees processPool
                in
                ( Node key subTrees_, processPool_ )
            else
                initElement element (killTree tree processPool)

        ( Element.Text s, Text ) ->
            ( Text, processPool )

        _  ->
            initElement element (killTree tree processPool)


killTrees nodes processPool =
    case nodes of
        [] ->
            processPool

        node :: restNodes ->
            killTrees restNodes (killTree node processPool)


killTree node processPool =
    case Debug.log "node" node of
        Component pid _ subTree ->
            killTree subTree (Pool.remove pid processPool)

        Node _ children ->
            killTrees children processPool

        Text ->
            processPool


diff : Model -> Model
diff { prevTree, processes } =
    let
        ( newPrevTree, newProcesses ) =
            diffElement app prevTree processes
    in
    { prevTree = newPrevTree
    , processes = newProcesses
    }
