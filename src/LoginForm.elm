module LoginForm exposing (..)

import Html

type alias Model =
    { username : String
    , password : String
    , loading : Bool
    }

type Msg
    = UsernameChanged String
    | PasswordChanged String
    | LoginClicked

init =
    { username = "", password = "", loading = False }

update msg model =
    case msg of
        UsernameChanged v -> { model | username = v }
        PasswordChanged v -> { model | password = v }
        LoginClicked -> { model | loading = True }

view { loading, div, inputText, inputPassword, button } toSelf model =
    if model.loading then
        loading
    else
        div
            []
            [ inputText { onChange = toSelf << UsernameChanged, value = model.username, label = "Username" }
            , inputPassword { onChange = toSelf << PasswordChanged, value = model.password, label = "Password" }
            , button { onClick = toSelf LoginClicked, label = "Login" }
            ]
