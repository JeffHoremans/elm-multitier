module Server.Console exposing (..)

import Task exposing (Task, andThen)

import Multitier.Error exposing (Error(..))

import Native.Server.Console

log : value -> Task Error ()
log value = (Native.Server.Console.log value) |> Task.mapError (\err -> ServerError err)
