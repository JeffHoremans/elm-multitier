effect module Multitier.Callstack where { command = MyCmd, subscription = MySub }
  exposing ( addProcedure, callProcedure, onCallComplete )

import Task exposing (Task, andThen, onError)
import Dict exposing (Dict)
import Json.Encode as Encode exposing (Value)
import Function.Extra exposing ((>>>>))

-- import Multitier.Error exposing (Error(..))
import Multitier.Procedure exposing (RemoteProcedure(..), Procedure(..), Identifier, Arguments(..))
import HttpServer

-- EFFECT MANAGER

type MyCmd msg = AddProcedure RemoteProcedure | Call Identifier Arguments HttpServer.Request


addProcedure : RemoteProcedure -> Cmd msg
addProcedure pc =
  command (AddProcedure pc)

callProcedure : Identifier -> Arguments -> HttpServer.Request -> Cmd msg
callProcedure identifier arguments request = command (Call identifier arguments request)

cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap f cmd = case cmd of
  AddProcedure task -> AddProcedure task
  Call identifier arguments request -> Call identifier arguments request



type MySub msg = OnCallComplete (HttpServer.Request -> String -> Maybe Value -> msg)

onCallComplete : (HttpServer.Request -> String -> Maybe Value -> msg) -> Sub msg
onCallComplete tagger =
  subscription (OnCallComplete tagger)

subMap : (a -> b) -> MySub a -> MySub b
subMap func (OnCallComplete tagger) = OnCallComplete (tagger >>>> func)

type alias State msg = { callStack : Dict String Procedure
                       , subs: List (MySub msg)}

init : Task Never (State msg)
init = Task.succeed (State Dict.empty [])

onEffects : Platform.Router msg Msg -> List (MyCmd msg) -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router cmdLst subs { callStack } =
    case cmdLst of
        [] -> Task.succeed (State callStack subs)

        (AddProcedure (RP identifier procedure)) :: cmds -> onEffects router cmds subs (State (Debug.log "callstack" (Dict.insert identifier procedure callStack)) subs)

        (Call identifier arguments request) :: cmds ->
          let maybePc = Dict.get identifier callStack
          in (case maybePc of
            Just pc -> case pc of
              P0 task  -> task
              `andThen` (\value ->  Platform.sendToSelf router (CallComplete request "" (Just value)))
              `onError` (\err ->    Platform.sendToSelf router (CallComplete request "Remote procedure failed" Nothing))
              _ -> Platform.sendToSelf router (CallComplete request "TODO - not supported yet" Nothing)
              -- PC1 a m1  ->
              -- PC2 a b m2  ->
              -- PC3 a b c m3  ->
              -- PC4 a b c d m4  ->
              -- PC5 a b c d e m5  ->
              -- PC6 a b c d e f m6  ->
              -- PC7 a b c d e f g m7 ->
            _ ->                    Platform.sendToSelf router (CallComplete request "No procedure found for identifier" Nothing))

            `andThen` \_ -> onEffects router cmds subs (State callStack subs)


-- HANDLE SELF MESSAGES

type Msg = CallComplete HttpServer.Request String (Maybe Value)

onSelfMsg : Platform.Router msg Msg -> Msg -> State msg -> Task Never (State msg)
onSelfMsg router selfMsg state =
  case selfMsg of
    CallComplete request message value ->
      let requests = List.map (\(OnCallComplete tagger) -> Platform.sendToApp router (tagger request message value)) state.subs
      in  Task.sequence requests
            `andThen` \_ -> Task.succeed state
