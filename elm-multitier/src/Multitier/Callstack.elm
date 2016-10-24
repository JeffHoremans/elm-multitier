effect module Multitier.Callstack where { command = MyCmd, subscription = MySub }
  exposing ( addCall, call, onCallComplete )

import Task exposing (Task, andThen, onError)
import Dict exposing (Dict)
import Json.Encode as Encode exposing (Value)
import Function.Extra exposing ((>>>>))

import Multitier.Error exposing (Error(..))
import HttpServer

-- EFFECT MANAGER

type MyCmd msg = AddCall (Task Error Value) | Call Int HttpServer.Request


addCall : (Task Error Value) -> Cmd msg
addCall task =
  command (AddCall task)

call : Int -> HttpServer.Request -> Cmd msg
call identifier request = command (Call identifier request)

cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap f cmd = case cmd of
  AddCall task -> AddCall task
  Call identifier request -> Call identifier request



type MySub msg = OnCallComplete (HttpServer.Request -> String -> Maybe Value -> msg)

onCallComplete : (HttpServer.Request -> String -> Maybe Value -> msg) -> Sub msg
onCallComplete tagger =
  subscription (OnCallComplete tagger)

subMap : (a -> b) -> MySub a -> MySub b
subMap func (OnCallComplete tagger) = OnCallComplete (tagger >>>> func)

type alias State msg = { nextId : Int
                   , callStack : Dict Int (Task Error Value)
                   , subs: List (MySub msg)}

init : Task Never (State msg)
init = Task.succeed (State 0 Dict.empty [])

onEffects : Platform.Router msg Msg -> List (MyCmd msg) -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router cmdLst subs { nextId, callStack } =
    case cmdLst of
        [] -> Task.succeed (State nextId callStack subs)

        (AddCall task) :: cmds -> onEffects router cmds subs (State (nextId + 1) (Dict.insert nextId task callStack) subs)

        (Call identifier request) :: cmds ->
          let maybeTask = Dict.get identifier callStack
          in (case maybeTask of
            Just task -> task
              `andThen` (\value ->  Platform.sendToSelf router (CallComplete request "" (Just value)))
              `onError` (\err ->    Platform.sendToSelf router (CallComplete request "Remote procedure failed" Nothing))
            _ ->                    Platform.sendToSelf router (CallComplete request "No procedure found for identifier" Nothing))

            `andThen` \_ -> onEffects router cmds subs (State nextId callStack subs)


-- HANDLE SELF MESSAGES

type Msg = CallComplete HttpServer.Request String (Maybe Value)

onSelfMsg : Platform.Router msg Msg -> Msg -> State msg -> Task Never (State msg)
onSelfMsg router selfMsg state =
  case selfMsg of
    CallComplete request message value ->
      let requests = List.map (\(OnCallComplete tagger) -> Platform.sendToApp router (tagger request message value)) state.subs
      in  Task.sequence requests
            `andThen` \_ -> Task.succeed state
