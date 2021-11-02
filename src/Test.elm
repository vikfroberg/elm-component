module Test exposing (..)

import Html
import Html.Attributes as HA
import Html.Events as HE
import Browser
import LoginForm
import List.Extra
import Tree exposing (Tree)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }

type alias Model =
    Tree ComponentModel

-- type alias Msg =
--     Tree ComponentMsg
type alias Msg = ()

type ComponentModel
    = Stateless
    | LoginFormModel LoginForm.Model

type ComponentMsg
    = LoginFormMsg LoginForm.Msg

init =
    let
        components _ =
            { loginForm = \_ ->
                let m = LoginForm.init in
                Tree.node
                    (LoginFormModel m)
                    (LoginForm.view (components ()) identity m )
                    []
            , button = \_ _ -> Tree.leaf Stateless []
            , inputText = \_ _ -> Tree.leaf Stateless []
            , inputPassword = \_ _ -> Tree.leaf Stateless []
            , loading = \_ _ -> Tree.leaf Stateless []
            , div = \_ _ children -> Tree.leaf Stateless (List.map (\f -> f ()) children)
            -- , input
            -- , link
            -- , block "div"
            -- , inline "span"
            }
    in
    root (components ())

update msg model =
    model

view model =
    Html.text ""
    -- let
    --     components tree =
    --         { loginForm = \_ ->
    --             case tree of
    --                 Tree.Node (LoginFormModel m) subTree children ->
    --                     LoginForm.view (components subTree) (always ()) m

    --                 _ ->
    --                     Html.text ""
    --         , button =
    --             \{ onClick, label } ->
    --                 Html.button
    --                     [ HE.onClick onClick
    --                     ]
    --                     [ Html.text label ]
    --         , inputText =
    --             \{ onChange, value, label } ->
    --                 Html.label
    --                     []
    --                     [ Html.strong [] [ Html.text label ]
    --                     , Html.input
    --                         [ HA.type_ "text"
    --                         , HA.value value
    --                         , HE.onInput onChange
    --                         ]
    --                         []
    --                     ]
    --         , inputPassword =
    --             \{ onChange, value, label } ->
    --                 Html.label
    --                     []
    --                     [ Html.strong [] [ Html.text label ]
    --                     , Html.input
    --                         [ HA.type_ "password"
    --                         , HA.value value
    --                         , HE.onInput onChange
    --                         ]
    --                         []
    --                     ]
    --         , loading =
    --             -- TODO: Represent loading state Throw
    --             \_ -> Html.text "Loading..."
    --         , div =
    --             \attributes children ->
    --                 -- List.Extra.zip children (Tree.children tree)
    --                 --     |> List.map (\f t -> f (components t) (Tree.extract t))
    --                 Html.div attributes children
    --         }
    -- in
    -- root (components model)


--- Root


root { div, loginForm } =
    div
        [ HA.style "color" "red" ]
        [ loginForm
        ]
