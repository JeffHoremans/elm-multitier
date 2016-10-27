module Server.Console exposing (log)

import Task exposing (Task, andThen)

import Multitier.Procedure exposing (Procedure1, procedure1)
import Multitier.Error exposing (Error(..))
import Multitier.Type exposing (void, string)

import Native.Server.Console

nativeLog : value -> Task Error ()
nativeLog value = (Native.Server.Console.log value) |> Task.mapError (\err -> ServerError err)

log : String -> (Error -> msg) -> (() -> msg) -> Procedure1 String msg
log identifier onError onSuccess = procedure1 identifier onError onSuccess string void nativeLog
