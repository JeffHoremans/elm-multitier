module Multitier.Procedure exposing
  ( RemoteProcedure(..)
  , Handlers
  , remoteProcedure
  , map)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Task exposing (Task)

import Multitier.Error exposing (Error(..))
import Multitier.Type exposing (Type)

type RemoteProcedure msg = RP (Handlers msg) (Task Error Value)

type alias Handlers msg = ((Error -> msg),(Value -> msg))

mapHandlers : (a -> msg) -> Handlers a -> Handlers msg
mapHandlers f (onError, onSuccess) = ((onError >> f), (onSuccess >> f))

map : (a -> msg) -> RemoteProcedure a -> RemoteProcedure msg
map f (RP handlers task) = RP (mapHandlers f handlers) task

remoteProcedure : (Error -> msg) -> (result -> msg) -> Type result -> Task Error result -> RemoteProcedure msg
remoteProcedure onError onSuccess (decodeResult, encodeResult) task =
  let mappedOnSuccess = mapOnSuccess onError onSuccess decodeResult
      mappedTask = mapTask encodeResult task
  in RP (onError,mappedOnSuccess) mappedTask

mapTask : (result -> Value) -> Task Error result -> Task Error Value
mapTask encode task = Task.map encode task

mapOnSuccess : (Error -> msg) -> (a -> msg) -> Decoder a -> (Value -> msg)
mapOnSuccess onError onSuccess decoder =
  \value -> let res = Decode.decodeValue decoder value
    in case res of
      Ok result -> onSuccess result
      Err err -> onError (MultitierError err)
