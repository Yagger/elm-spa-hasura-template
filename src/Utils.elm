module Utils exposing (..)

import Process
import Task


delay : Float -> msg -> Cmd msg
delay millis msg =
  Process.sleep millis
  |> Task.perform (\_ -> msg)