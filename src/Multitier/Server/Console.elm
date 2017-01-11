module Multitier.Server.Console exposing (..)

import Task exposing (Task, andThen)

import Multitier.Error exposing (Error(..))

import Native.Multitier.Server.Console

log : value -> Task Error ()
log value = (Native.Multitier.Server.Console.log value) |> Task.mapError (\err -> ServerError err)
