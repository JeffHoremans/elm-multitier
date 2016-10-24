module Server.Console exposing (log)

import Task exposing (Task, andThen)

import Multitier exposing (MultitierCmd, performOnServer, encodeVoid, decodeVoid)
import Multitier.Error exposing (Error(..))

import Native.Server.Console

nativeLog : value -> Task Error ()
nativeLog value = (Native.Server.Console.log value) |> Task.mapError (\err -> ServerError err)

log : value -> (Error -> msg) -> (() -> msg) -> MultitierCmd msg
log val onError onSuccess = performOnServer onError onSuccess decodeVoid encodeVoid (nativeLog val)
