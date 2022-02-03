module Component exposing (..)

import Element exposing (Element)
import Html
import Counters
import Counter
import Button
import Return


type Props msg
    = CountersProps Counters.Props
    | CounterProps (Counter.Props msg)
    | ButtonProps (Button.Props msg)


type Model
    = CounterModel Counter.Model
    | CountersModel Counters.Model
    | ButtonModel Button.Model


type Msg
    = CounterMsg Counter.Msg
    | CountersMsg Counters.Msg
    | ButtonMsg Button.Msg


all =
    { counters = CountersProps >> Element.component
    , counter = CounterProps >> Element.component
    , button = ButtonProps >> Element.component
    }


isEqual : Props msg -> Props msg -> Bool
isEqual a b =
    case ( a, b ) of
        ( CountersProps _, CountersProps _ ) ->
            True

        ( CounterProps _, CounterProps _ ) ->
            True

        ( ButtonProps _, ButtonProps _ ) ->
            True

        _ ->
            False


init : Props msg -> (Msg -> msg) -> ( Model, Cmd msg )
init props toSelf =
    case props of
        CountersProps subProps ->
            Counters.init subProps (CountersMsg >> toSelf)
                |> Tuple.mapFirst CountersModel

        CounterProps subProps ->
            Counter.init subProps (CounterMsg >> toSelf)
                |> Tuple.mapFirst CounterModel

        ButtonProps subProps ->
            Button.init subProps (ButtonMsg >> toSelf)
                |> Tuple.mapFirst ButtonModel


propsChanged : Props msg -> Props msg -> (Msg -> msg) -> Model -> ( Model, Cmd msg )
propsChanged prevProps nextProps toSelf model =
    case ( prevProps, nextProps, model ) of
        ( CountersProps subPrevProps, CountersProps subNextProps, CountersModel subModel ) ->
            Counters.propsChanged subPrevProps subNextProps (CountersMsg >> toSelf) subModel
                |> Return.map CountersModel

        ( CounterProps subPrevProps, CounterProps subNextProps, CounterModel subModel ) ->
            Counter.propsChanged subPrevProps subNextProps (CounterMsg >> toSelf) subModel
                |> Return.map CounterModel

        ( ButtonProps subPrevProps, ButtonProps subNextProps, ButtonModel subModel ) ->
            Button.propsChanged subPrevProps subNextProps (ButtonMsg >> toSelf) subModel
                |> Return.map ButtonModel

        _ ->
            ( model, Cmd.none )


update : Props msg -> (Msg -> msg) -> Msg -> Model -> ( Model, Cmd msg )
update props toSelf msg model =
    case ( props, msg, model ) of
        ( CountersProps subProps, CountersMsg subMsg, CountersModel subModel ) ->
            Counters.update subProps (CountersMsg >> toSelf) subMsg subModel
                |> Return.map CountersModel

        ( CounterProps subProps, CounterMsg subMsg, CounterModel subModel ) ->
            Counter.update subProps (CounterMsg >> toSelf) subMsg subModel
                |> Return.map CounterModel

        ( ButtonProps subProps, ButtonMsg subMsg, ButtonModel subModel ) ->
            Button.update subProps (ButtonMsg >> toSelf) subMsg subModel
                |> Return.map ButtonModel

        _ ->
            ( model, Cmd.none )


-- TODO: Is it possible to remove Html.Attribute from here?
view : Props msg -> (Msg -> msg) -> Model -> Element (Html.Attribute msg) (Props msg)
view props toSelf model =
    case ( props, model ) of
        ( CountersProps _, CountersModel subModel ) ->
            Counters.view all (CountersMsg >> toSelf) subModel

        ( CounterProps _, CounterModel subModel ) ->
            Counter.view all (CounterMsg >> toSelf) subModel

        ( ButtonProps _, ButtonModel subModel ) ->
            Button.view all (ButtonMsg >> toSelf) subModel

        _ ->
            Element.none
