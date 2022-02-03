module System exposing (..)

import Element exposing (Element)
import Pool exposing (Pool)
import List.Extra
import Return
import Triple


-- TODO: Use state monad
-- TODO: Do not run propsChanged for all components


type alias Model attribute props model =
    { tree : Tree attribute props
    , processes : Pool model
    }


type alias ComponentConfig attribute props msg model =
    { init : props -> (msg -> Msg msg) -> ( model, Cmd (Msg msg) )
    , update : props -> (msg -> Msg msg ) -> msg -> model -> ( model, Cmd (Msg msg) )
    , propsChanged : props -> props -> (msg -> Msg msg) -> model -> ( model, Cmd (Msg msg) )
    , view : props -> (msg -> Msg msg) -> model -> Element attribute props
    , isEqual : props -> props -> Bool
    }


type alias ViewConfig attribute element =
    { text : String -> element
    , node : String -> List attribute -> List element -> element
    }


type Msg msg
    = SendToProcess Int msg
    | NoOp


type Tree attribute props
    = Component Pool.Id props (Tree attribute props)
    | Node String (List attribute) (List (Tree attribute props))
    | Text String


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


init :
    ComponentConfig attribute props msg model
    -> Element attribute props
    -> ( Model attribute props model, Cmd (Msg msg) )
init config app =
    let
        ( tree, processes, cmd ) =
            initElement config app Pool.empty
    in
    ( { tree = tree, processes = processes }
    , cmd
    )


initElement :
    ComponentConfig attribute props msg model
    -> Element attribute props
    -> Pool model
    -> ( Tree attribute props, Pool model, Cmd (Msg msg) )
initElement config element processes =
    case element of
        Element.Component props ->
            let
                ( pid, processes1 ) = Pool.alloc processes
                toSelf = SendToProcess pid
                ( model, cmd ) = config.init props toSelf
                processes2 = Pool.replace pid model processes1
                ( tree, processes3, cmds ) =
                    initElement config (config.view props toSelf model) processes2
            in
            ( Component pid props tree
            , processes3
            , Cmd.batch [ cmd, cmds ]
            )

        Element.Node t attributes children ->
            let
                ( tree, processes1, cmd ) =
                    initChildren config children processes
            in
            ( Node t attributes tree
            , processes1
            , cmd
            )

        Element.Text text ->
            ( Text text, processes, Cmd.none )


initChildren config children processes =
    List.foldl
        (\el ( kids, accProcesses, cmd ) ->
            let
                ( tree, accProcesses_, cmd_ ) = initElement config el accProcesses
            in
            ( kids ++ [ tree ], accProcesses_, Cmd.batch [ cmd, cmd_ ] )
        )
        ( [], processes, Cmd.none )
        children


view : ViewConfig attribute element -> Model attribute props model -> element
view config model =
    let
        go tree =
            case tree of
                Component pid props subTree ->
                    go subTree

                Node key attributes subTrees ->
                    config.node key attributes (List.map go subTrees)

                Text s ->
                    config.text s
    in
    go model.tree


update :
    ComponentConfig attribute props msg model
    -> Element attribute props
    -> Msg msg
    -> Model attribute props model
    -> ( Model attribute props model, Cmd (Msg msg) )
update config app msg model =
    case msg of
        SendToProcess pid componentMsg ->
            let
                maybeComponentModel = Pool.get pid model.processes
                maybeComponentProps = treeFindPid pid model.tree
                toSelf = SendToProcess pid
            in
            case ( maybeComponentModel, maybeComponentProps ) of
                ( Just componentModel, Just componentProps ) ->
                    config.update componentProps toSelf componentMsg componentModel
                        |> Return.map (\newComponentModel -> { model | processes = Pool.replace pid newComponentModel model.processes })
                        |> Return.andThen (diff config app)

                _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


diffElement :
    ComponentConfig attribute props msg model
    -> Element attribute props
    -> Tree attribute props
    -> Pool model
    -> ( Tree attribute props, Pool model, Cmd (Msg msg) )
diffElement config element tree processes =
    case ( element, tree ) of
        ( Element.Component elementProps, Component pid treeProps subTree ) ->
            if config.isEqual elementProps treeProps then
                let
                    toSelf = SendToProcess pid
                    maybeComponentModel = Pool.get pid processes
                in
                case maybeComponentModel of
                    Just componentModel ->
                        let
                            ( componentModel_, cmd ) = config.propsChanged treeProps elementProps toSelf componentModel
                            processes_ = Pool.replace pid componentModel_ processes
                        in
                        diffElement config (config.view elementProps toSelf componentModel) subTree processes_
                            |> Triple.mapFirst (Component pid elementProps)

                    _ ->
                        ( tree, processes, Cmd.none )
            else
                initElement config element (killTree tree processes)

        ( Element.Node elementKey attributes subElements, Node treeKey _ subTrees ) ->
            if elementKey == treeKey then
                let
                    ( subTrees_, processes_, cmd ) =
                        diffChildren config subElements subTrees processes
                in
                ( Node elementKey attributes subTrees_, processes_, cmd )
            else
                initElement config element (killTree tree processes)

        ( Element.Text s, Text _ ) ->
            ( Text s, processes, Cmd.none )

        _  ->
            initElement config element (killTree tree processes)


diffChildren config elements trees processes =
    if List.length elements > List.length trees then
        let
            ( children, processes_, cmd_ ) =
                diffChildren config (List.take (List.length trees) elements) trees processes

            ( restChildren, processes__, cmd__ ) =
                initChildren config (List.drop (List.length trees) elements) processes_
        in
        ( children ++ restChildren, processes__, Cmd.batch [ cmd_, cmd__ ] )
    else if List.length elements < List.length trees then
        let
            ( children, processes_, cmd_ ) =
                diffChildren config elements (List.take (List.length elements) trees) processes
        in
        ( children, killTrees (List.drop (List.length elements) trees) processes_, cmd_ )
    else
        List.foldl
            (\( e, t ) ( kids, p, c ) ->
                let
                    ( e_, p_, c_ ) = diffElement config e t p
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


diff :
    ComponentConfig attribute props msg model
    -> Element attribute props
    -> Model attribute props model
    -> ( Model attribute props model, Cmd (Msg msg) )
diff config app { tree, processes } =
    let
        ( newTree, newProcesses, cmd ) =
            diffElement config app tree processes
    in
    ( { tree = newTree, processes = newProcesses }
    , cmd
    )
