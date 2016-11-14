module Multitier.Procedure exposing
  ( RemoteProcedure(..)
  , Handlers
  , remoteProcedure
  , map)

import Json.Encode as Encode exposing (Value)
import Task exposing (Task)

import Native.Multitier
import Multitier.Error exposing (Error(..))

type RemoteProcedure msg = RP (Handlers msg) (Task Error Value)

type alias Handlers msg = ((Error -> msg),(Value -> msg))

mapHandlers : (a -> msg) -> Handlers a -> Handlers msg
mapHandlers f (onError, onSuccess) = ((onError >> f), (onSuccess >> f))

map : (a -> msg) -> RemoteProcedure a -> RemoteProcedure msg
map f (RP handlers task) = RP (mapHandlers f handlers) task

remoteProcedure : (Error -> msg) -> (result -> msg) -> Task Error result -> RemoteProcedure msg
remoteProcedure onError onSuccess task =
  let mappedOnSuccess = \value -> let res = fromJSON value in onSuccess res
      mappedTask = Task.map toJSON task
  in RP (onError,mappedOnSuccess) mappedTask

toJSON : a -> Value
toJSON = Native.Multitier.toJSON

fromJSON : Value -> a
fromJSON = Native.Multitier.fromJSON
