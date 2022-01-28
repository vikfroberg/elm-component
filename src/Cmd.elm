module Cmd exposing (..)

import Task
import Process


send : a -> Cmd a
send a =
    Task.succeed a
        |> Task.perform identity


delay : Float -> a -> Cmd a
delay timeout a =
    Process.sleep timeout
        |> Task.map (\_ -> a)
        |> Task.perform identity
