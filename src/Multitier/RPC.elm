module Multitier.RPC exposing
  ( RPC(..)
  , Handler
  , rpc
  , map)

import Json.Encode as Encode exposing (Value)
import Task exposing (Task)

import Multitier.Error exposing (Error(..))
import Multitier.LowLevel exposing (toJSON, fromJSON)

type RPC serverModel msg serverMsg = Rpc (Handler msg) (serverModel -> (serverModel, Task Error Value, Cmd serverMsg))

type alias Handler msg = Result Error Value -> msg

mapHandlers : (a -> msg) -> Handler a -> Handler msg
mapHandlers f handler = handler >> f

mapUpdate : (b -> serverMsg) -> (a -> serverModel -> serverModel) -> (serverModel -> a) -> (a -> (a, Task Error Value, Cmd b)) -> (serverModel -> (serverModel, Task Error Value, Cmd serverMsg))
mapUpdate toServerMsg updateModel toA toTask = \serverModel -> let a = (toA serverModel) in
                                                let (newA, task, cmd) = toTask a in (updateModel newA serverModel, task, Cmd.map toServerMsg cmd)

map : (b -> msg) -> (c -> serverMsg) -> (a -> serverModel -> serverModel) -> (serverModel -> a) -> RPC a b c -> RPC serverModel msg serverMsg
map toMsg toServerMsg fromA toA (Rpc handlers update) = Rpc (mapHandlers toMsg handlers) (mapUpdate toServerMsg fromA toA update)

rpc : (Result Error result -> msg) -> (serverModel -> (serverModel, Task Error result, Cmd serverMsg)) -> RPC serverModel msg serverMsg
rpc handler update =
  let mappedHandler = \result -> handler (Result.map fromJSON result)
      mappedUpdate = \serverModel -> let (newServerModel, task, cmd) = update serverModel
                                     in (newServerModel, Task.map toJSON task, cmd)
  in Rpc mappedHandler mappedUpdate
