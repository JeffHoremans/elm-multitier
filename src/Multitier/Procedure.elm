module Multitier.Procedure exposing
  ( RemoteProcedure(..)
  , Handlers
  , remoteProcedure
  , map)

import Json.Encode as Encode exposing (Value)
import Task exposing (Task)

import Native.Multitier
import Multitier.Error exposing (Error(..))

type RemoteProcedure serverModel msg = RP (Handlers msg) (serverModel -> (serverModel, Task Error Value))

type alias Handlers msg = ((Error -> msg),(Value -> msg))

mapHandlers : (a -> msg) -> Handlers a -> Handlers msg
mapHandlers f (onError, onSuccess) = ((onError >> f), (onSuccess >> f))

mapToTask : (a -> serverModel -> serverModel) -> (serverModel -> a) -> (a -> (a, Task Error Value)) -> (serverModel -> (serverModel, Task Error Value))
mapToTask updateModel toA toTask = \serverModel -> let a = (toA serverModel) in
                                                let (newA, task) = toTask a in (updateModel newA serverModel, task)

map : (b -> msg) -> (a -> serverModel -> serverModel) -> (serverModel -> a) -> RemoteProcedure a b -> RemoteProcedure serverModel msg
map toMsg fromA toA (RP handlers toTask) = RP (mapHandlers toMsg handlers) (mapToTask fromA toA toTask)

remoteProcedure : (Error -> msg) -> (result -> msg) -> (serverModel -> (serverModel, Task Error result)) -> RemoteProcedure serverModel msg
remoteProcedure onError onSuccess toTask =
  let mappedOnSuccess = \value -> let res = fromJSON value in onSuccess res
      mappedToTask = \serverModel -> let (newServerModel, task) = toTask serverModel
                                     in (newServerModel, Task.map toJSON task)
  in RP (onError,mappedOnSuccess) mappedToTask

toJSON : a -> Value
toJSON = Native.Multitier.toJSON

fromJSON : Value -> a
fromJSON = Native.Multitier.fromJSON
