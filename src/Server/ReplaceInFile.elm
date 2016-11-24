module Server.ReplaceInFile exposing (..)

import Task exposing (Task, andThen)

import Multitier.Error exposing (Error(..))

import Native.Server.ReplaceInFile

replace : String -> String -> String -> Bool -> Task Error ()
replace file regex replacement allowEmptyPaths = (Native.Server.ReplaceInFile.replace file regex replacement allowEmptyPaths) |> Task.mapError (\err -> ServerError err)
