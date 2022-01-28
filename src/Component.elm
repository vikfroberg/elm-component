module Component exposing (..)

import Element exposing (Element)
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
    { counters = CountersProps >> Element.Component
    , counter = CounterProps >> Element.Component
    , button = ButtonProps >> Element.Component
    }


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


init c toSelf =
    case c of
        CountersProps props ->
            Counters.init props (CountersMsg >> toSelf)
                |> Tuple.mapFirst CountersModel

        CounterProps props ->
            Counter.init props (CounterMsg >> toSelf)
                |> Tuple.mapFirst CounterModel

        ButtonProps props ->
            Button.init props (ButtonMsg >> toSelf)
                |> Tuple.mapFirst ButtonModel


propsChanged prevProps nextProps toSelf model =
    case ( prevProps, nextProps, model ) of
        ( CountersProps prevProps_, CountersProps nextProps_, CountersModel model_ ) ->
            Counters.propsChanged prevProps_ nextProps_ (CountersMsg >> toSelf) model_
                |> Return.map CountersModel

        ( CounterProps prevProps_, CounterProps nextProps_, CounterModel model_ ) ->
            Counter.propsChanged prevProps_ nextProps_ (CounterMsg >> toSelf) model_
                |> Return.map CounterModel

        ( ButtonProps prevProps_, ButtonProps nextProps_, ButtonModel model_ ) ->
            Button.propsChanged prevProps_ nextProps_ (ButtonMsg >> toSelf) model_
                |> Return.map ButtonModel

        _ ->
            ( model, Cmd.none )


update props toSelf msg model =
    case Debug.log "Component.update" ( props, msg, model ) of
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
