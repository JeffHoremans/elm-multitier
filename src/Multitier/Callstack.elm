effect module Multitier.Callstack where { command = MyCmd, subscription = MySub }
  exposing ( addProcedure, callProcedure, onCallComplete )

import Task exposing (Task, andThen, onError)
import Dict exposing (Dict)
import Json.Encode as Encode exposing (Value)
import Function.Extra exposing ((>>>>))

-- import Multitier.Error exposing (Error(..))
import Multitier.Procedure exposing (RemoteProcedure(..), Procedure(..), Identifier, Arguments(..))
import Multitier.Error exposing (Error)
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

sendSuccess: Platform.Router msg Msg -> HttpServer.Request -> String -> Value -> Task x ()
sendSuccess router request message value = sendCallComplete router request message (Just value)

sendError: Platform.Router msg Msg -> HttpServer.Request -> String -> Task x ()
sendError router request message = sendCallComplete router request message Nothing

sendCallComplete: Platform.Router msg Msg -> HttpServer.Request -> String -> Maybe Value -> Task x ()
sendCallComplete router request message value = Platform.sendToSelf router (CallComplete request message value)

performProcedure: Platform.Router msg Msg -> HttpServer.Request -> Task Error Value -> Task x ()
performProcedure router request task = task
  `andThen` (\value ->  sendSuccess router request "" value)
  `onError` (\err ->    sendError router request "Remote procedure failed")

onEffects : Platform.Router msg Msg -> List (MyCmd msg) -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router cmdLst subs { callStack } =
    case cmdLst of
        [] -> Task.succeed (State callStack subs)

        (AddProcedure (RP identifier procedure)) :: cmds -> onEffects router cmds subs (State (Debug.log "callstack" (Dict.insert identifier procedure callStack)) subs)

        (Call identifier arguments request) :: cmds ->
          let maybePc = Dict.get identifier callStack
          in (case maybePc of
            Just pc -> case pc of
              P0 task  -> performProcedure router request task
              
              P1 taskf -> case arguments of
                A1 a -> performProcedure router request (taskf a)
                _ -> sendError router request "Invalid number of arguments for remote procedure"

              P2 taskf -> case arguments of
                A2 a b -> performProcedure router request (taskf a b)
                _ -> sendError router request "Invalid number of arguments for remote procedure"

              P3 taskf -> case arguments of
                A3 a b c -> performProcedure router request (taskf a b c)
                _ -> sendError router request "Invalid number of arguments for remote procedure"

              P4 taskf -> case arguments of
                A4 a b c d -> performProcedure router request (taskf a b c d)
                _ -> sendError router request "Invalid number of arguments for remote procedure"

              P5 taskf -> case arguments of
                A5 a b c d e -> performProcedure router request (taskf a b c d e)
                _ -> sendError router request "Invalid number of arguments for remote procedure"

              P6 taskf -> case arguments of
                A6 a b c d e f -> performProcedure router request (taskf a b c d e f)
                _ -> sendError router request "Invalid number of arguments for remote procedure"

              P7 taskf -> case arguments of
                A7 a b c d e f g -> performProcedure router request (taskf a b c d e f g)
                _ -> sendError router request "Invalid number of arguments for remote procedure"

            _ -> sendError router request "No procedure found for identifier" )

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
