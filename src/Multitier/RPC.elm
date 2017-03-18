module Multitier.RPC exposing
  ( RPC(..)
  , Handler
  , rpc
  , map
  , map2)

import Json.Encode as Encode exposing (Value)
import Task exposing (Task)

import Multitier.Error exposing (Error(..))
import Multitier.LowLevel as LowLevel exposing (toJSON, fromJSON)

type RPC serverModel msg serverMsg = Rpc (Handler msg) (serverModel -> (serverModel, Task Error Value, Cmd serverMsg))

type alias Handler msg = Result Error Value -> msg

mapHandlers : (a -> msg) -> Handler a -> Handler msg
mapHandlers f handler = handler >> f

mapUpdate : (a -> serverModel -> (serverModel, Cmd serverMsg, (b -> serverMsg))) -> (serverModel -> a) -> (a -> (a, Task Error Value, Cmd b)) -> (serverModel -> (serverModel, Task Error Value, Cmd serverMsg))
mapUpdate updateModel toA toTask = \serverModel ->
  let a = (toA serverModel) in
    let (newA, task, cmd) = toTask a in
      let (newModel, newCmd, toServerMsg) = updateModel newA serverModel in
        (newModel, task, Cmd.batch [newCmd, Cmd.map toServerMsg cmd])

map : (b -> msg) -> (a -> serverModel -> (serverModel, Cmd serverMsg, ((c -> serverMsg)))) -> (serverModel -> a) -> RPC a b c -> RPC serverModel msg serverMsg
map toMsg fromA toA (Rpc handlers update) = Rpc (mapHandlers toMsg handlers) (mapUpdate fromA toA update)

rpc : (Result Error result -> msg) -> (serverModel -> (serverModel, Task Error result, Cmd serverMsg)) -> RPC serverModel msg serverMsg
rpc handler update =
  let mappedHandler = \result -> handler (Result.map fromJSON result)
      mappedUpdate = \serverModel -> let (newServerModel, task, cmd) = update serverModel
                                     in (newServerModel, Task.map toJSON task, cmd)
  in Rpc mappedHandler mappedUpdate

map2 : (b -> msg) -> ((a -> (a,Cmd c)) -> serverModel -> (serverModel, Cmd serverMsg)) -> (serverModel -> a) -> RPC a b c -> RPC serverModel msg serverMsg
map2 toMsg wrapUpdate toA (Rpc handlers update) = Rpc (mapHandlers toMsg handlers) (mapUpdate2 wrapUpdate toA update)

mapUpdate2 : ((a -> (a,Cmd c)) -> serverModel -> (serverModel, Cmd serverMsg)) -> (serverModel -> a) -> (a -> (a, Task Error Value, Cmd c)) -> (serverModel -> (serverModel, Task Error Value, Cmd serverMsg))
mapUpdate2 wrapUpdate toA update = \serverModel ->
  let updateA a = let (newA,_,cmd) = update a in (newA,cmd)
      (_,task,_) = update (toA serverModel) in
    let (newServerModel,newServerCmd) = wrapUpdate updateA serverModel in
      (newServerModel, task, newServerCmd)
