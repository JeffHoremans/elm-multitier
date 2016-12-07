module Multitier.Procedure exposing
  ( Procedure(..)
  , Handler
  , procedure
  , map)

import Json.Encode as Encode exposing (Value)
import Task exposing (Task)

import Multitier.Error exposing (Error(..))
import Multitier.LowLevel exposing (toJSON, fromJSON)

type Procedure serverModel msg = Proc (Handler msg) (serverModel -> (serverModel, Task Error Value))

type alias Handler msg = Result Error Value -> msg

mapHandlers : (a -> msg) -> Handler a -> Handler msg
mapHandlers f handler = handler >> f

mapToTask : (a -> serverModel -> serverModel) -> (serverModel -> a) -> (a -> (a, Task Error Value)) -> (serverModel -> (serverModel, Task Error Value))
mapToTask updateModel toA toTask = \serverModel -> let a = (toA serverModel) in
                                                let (newA, task) = toTask a in (updateModel newA serverModel, task)

map : (b -> msg) -> (a -> serverModel -> serverModel) -> (serverModel -> a) -> Procedure a b -> Procedure serverModel msg
map toMsg fromA toA (Proc handlers toTask) = Proc (mapHandlers toMsg handlers) (mapToTask fromA toA toTask)

procedure : (Result Error result -> msg) -> (serverModel -> (serverModel, Task Error result)) -> Procedure serverModel msg
procedure handler toTask =
  let mappedHandler = \result -> handler (Result.map fromJSON result)
      mappedToTask = \serverModel -> let (newServerModel, task) = toTask serverModel
                                     in (newServerModel, Task.map toJSON task)
  in Proc mappedHandler mappedToTask
