module Multitier.Procedure exposing
  ( RemoteProcedure(..)
  , Handler
  , remoteProcedure
  , map)

import Json.Encode as Encode exposing (Value)
import Task exposing (Task)

import Multitier.Error exposing (Error(..))
import Multitier.LowLevel exposing (toJSON, fromJSON)

type RemoteProcedure serverModel msg = RP (Handler msg) (serverModel -> (serverModel, Task Error Value))

type alias Handler msg = Result Error Value -> msg

mapHandlers : (a -> msg) -> Handler a -> Handler msg
mapHandlers f handler = handler >> f

mapToTask : (a -> serverModel -> serverModel) -> (serverModel -> a) -> (a -> (a, Task Error Value)) -> (serverModel -> (serverModel, Task Error Value))
mapToTask updateModel toA toTask = \serverModel -> let a = (toA serverModel) in
                                                let (newA, task) = toTask a in (updateModel newA serverModel, task)

map : (b -> msg) -> (a -> serverModel -> serverModel) -> (serverModel -> a) -> RemoteProcedure a b -> RemoteProcedure serverModel msg
map toMsg fromA toA (RP handlers toTask) = RP (mapHandlers toMsg handlers) (mapToTask fromA toA toTask)

remoteProcedure : (Result Error result -> msg) -> (serverModel -> (serverModel, Task Error result)) -> RemoteProcedure serverModel msg
remoteProcedure handler toTask =
  let mappedHandler = \result -> handler (Result.map fromJSON result)
      mappedToTask = \serverModel -> let (newServerModel, task) = toTask serverModel
                                     in (newServerModel, Task.map toJSON task)
  in RP mappedHandler mappedToTask
