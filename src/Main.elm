module Main exposing (..)

import Browser
import Element
import Component
import Pool exposing (Pool)
import Html exposing (Html)
import Counter
import Counters
import Button
import List.Extra
import Return
import Triple


app =
    Element.component (Component.CountersProps {})

type alias Model =
    { tree : Tree
    , processes : Processes
    }

type Msg
    = SendToProcess Int Component.Msg
    | NoOp

type alias Props =
    Component.Props Msg

type alias Element =
    Element.Element Msg Props

type alias Processes =
    Pool Component.Model

type Tree
    = Component Pool.Id Props Tree
    | Node String (List (Html.Attribute Msg)) (List Tree)
    | Text String


treeFindPid : Pool.Id -> Tree -> Maybe Props
treeFindPid pid tree =
    case tree of
        Component subPid props subTree ->
            if pid == subPid then
                Just props
            else
                treeFindPid pid subTree

        Node _ _ subTrees ->
            List.filterMap (treeFindPid pid) subTrees
                |> List.head

        Text _ ->
            Nothing


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd Msg )
init =
    let
        ( tree, processes, cmd ) =
            initElement app Pool.empty
    in
    ( { tree = tree, processes = processes }
    , cmd
    )


initElement : Element -> Processes -> ( Tree, Processes, Cmd Msg )
initElement element processes =
    case element of
        Element.Component props ->
            let
                ( pid, processes1 ) = Pool.alloc processes
                toSelf = SendToProcess pid
                ( model, cmd ) = Component.init props toSelf
                processes2 = Pool.replace pid model processes1
                ( tree, processes3, cmds ) =
                    initElement (Component.view props toSelf model) processes2
            in
            ( Component pid props tree
            , processes3
            , Cmd.batch [ cmd, cmds ]
            )

        Element.Node t attributes children ->
            let
                ( tree, processes1, cmd ) =
                    initChildren children processes
            in
            ( Node t attributes tree
            , processes1
            , cmd
            )

        Element.Text text ->
            ( Text text, processes, Cmd.none )


initChildren children processes =
    List.foldl
        (\el ( kids, accProcesses, cmd ) ->
            let
                ( tree, accProcesses_, cmd_ ) = initElement el accProcesses
            in
            ( kids ++ [ tree ], accProcesses_, Cmd.batch [ cmd, cmd_ ] )
        )
        ( [], processes, Cmd.none )
        children


view : Model -> Html Msg
view model =
    let
        go tree =
            case tree of
                Component pid props subTree ->
                    go subTree

                Node key attributes subTrees ->
                    Html.node key attributes (List.map go subTrees)

                Text s ->
                    Html.text s
    in
    go model.tree


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "Main.update" msg of
        SendToProcess pid componentMsg ->
            let
                maybeComponentModel = Pool.get pid model.processes
                maybeComponentProps = treeFindPid pid model.tree
                toSelf = SendToProcess pid
            in
            case ( maybeComponentModel, maybeComponentProps ) of
                ( Just componentModel, Just componentProps ) ->
                    Component.update componentProps toSelf componentMsg componentModel
                        |> Return.map (\process -> { model | processes = Pool.replace pid process model.processes })
                        |> Return.andThen diff

                _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


-- TODO: Should this take the model?
diffElement : Element -> Tree -> Processes -> ( Tree, Processes, Cmd Msg )
diffElement element tree processes =
    case ( element, tree ) of
        ( Element.Component elementProps, Component pid treeProps subTree ) ->
            if Component.isEqual elementProps treeProps then
                let
                    toSelf = SendToProcess pid
                    maybeComponentModel = Pool.get pid processes
                in
                case maybeComponentModel of
                    Just componentModel ->
                        let
                            ( componentModel_, cmd ) = Component.propsChanged treeProps elementProps toSelf componentModel
                            processes_ = Pool.replace pid componentModel_ processes
                        in
                        diffElement (Component.view elementProps toSelf componentModel) subTree processes_
                            |> Triple.mapFirst (Component pid elementProps)

                    _ ->
                        Debug.todo "should never happen"
                        -- ( tree, processes )
            else
                initElement element (killTree tree processes)

        ( Element.Node elementKey attributes subElements, Node treeKey _ subTrees ) ->
            if elementKey == treeKey then
                let
                    ( subTrees_, processes_, cmd ) =
                        diffChildren subElements subTrees processes
                in
                ( Node elementKey attributes subTrees_, processes_, cmd )
            else
                initElement element (killTree tree processes)

        ( Element.Text s, Text _ ) ->
            ( Text s, processes, Cmd.none )

        _  ->
            initElement element (killTree tree processes)


diffChildren : List Element -> List Tree -> Processes -> ( List Tree, Processes, Cmd Msg )
diffChildren elements trees processes =
    if List.length elements > List.length trees then
        let
            ( children, processes_, cmd_ ) =
                diffChildren (List.take (List.length trees) elements) trees processes

            ( restChildren, processes__, cmd__ ) =
                initChildren (List.drop (List.length trees) elements) processes_
        in
        ( children ++ restChildren, processes__, Cmd.batch [ cmd_, cmd__ ] )
    else if List.length elements < List.length trees then
        let
            ( children, processes_, cmd_ ) =
                diffChildren elements (List.take (List.length elements) trees) processes
        in
        ( children, killTrees (List.drop (List.length elements) trees) processes_, cmd_ )
    else
        List.foldl
            (\( e, t ) ( kids, p, c ) ->
                let
                    ( e_, p_, c_ ) = diffElement e t p
                in
                ( kids ++ [ e_ ], p_, Cmd.batch [ c, c_ ] )
            )
            ( [], processes, Cmd.none )
            (List.Extra.zip elements trees)


killTrees nodes processes =
    case nodes of
        [] ->
            processes

        node :: restNodes ->
            killTrees restNodes (killTree node processes)


killTree node processes =
    case node of
        Component pid _ subTree ->
            killTree subTree (Pool.remove pid processes)

        Node _ _ children ->
            killTrees children processes

        Text _ ->
            processes


diff : Model -> ( Model, Cmd Msg )
diff { tree, processes } =
    let
        ( newTree, newProcesses, cmd ) =
            diffElement app tree processes
    in
    ( { tree = newTree, processes = newProcesses }
    , cmd
    )
