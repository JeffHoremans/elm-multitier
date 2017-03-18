module Multitier.RPC exposing
  ( RPC(..)
  , Handler
  , rpc
  , map)

import Json.Encode as Encode exposing (Value)
import Task exposing (Task)

import Multitier.Error exposing (Error(..))
import Multitier.LowLevel as LowLevel exposing (toJSON, fromJSON)

type RPC serverModel msg serverMsg = Rpc (Handler msg) (serverModel -> (serverModel, Task Error Value, Cmd serverMsg))

type alias Handler msg = Result Error Value -> msg

mapHandlers : (a -> msg) -> Handler a -> Handler msg
mapHandlers f handler = handler >> f

rpc : (Result Error result -> msg) -> (serverModel -> (serverModel, Task Error result, Cmd serverMsg)) -> RPC serverModel msg serverMsg
rpc handler update =
  let mappedHandler = \result -> handler (Result.map fromJSON result)
      mappedUpdate = \serverModel -> let (newServerModel, task, cmd) = update serverModel
                                     in (newServerModel, Task.map toJSON task, cmd)
  in Rpc mappedHandler mappedUpdate

map : (b -> msg) -> ((a -> (a,Cmd c)) -> serverModel -> (serverModel, Cmd serverMsg)) -> (serverModel -> a) -> RPC a b c -> RPC serverModel msg serverMsg
map toMsg wrapUpdate toA (Rpc handlers update) = Rpc (mapHandlers toMsg handlers) (mapUpdate wrapUpdate toA update)

mapUpdate : ((a -> (a,Cmd c)) -> serverModel -> (serverModel, Cmd serverMsg)) -> (serverModel -> a) -> (a -> (a, Task Error Value, Cmd c)) -> (serverModel -> (serverModel, Task Error Value, Cmd serverMsg))
mapUpdate wrapUpdate toA update = \serverModel ->
  let updateA a = let (newA,_,cmd) = update a in (newA,cmd)
      (_,task,_) = update (toA serverModel) in
    let (newServerModel,newServerCmd) = wrapUpdate updateA serverModel in
      (newServerModel, task, newServerCmd)
