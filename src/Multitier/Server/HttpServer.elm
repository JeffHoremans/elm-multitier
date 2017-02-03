effect module Multitier.Server.HttpServer where { command = MyCmd, subscription = MySub } exposing
  ( reply
  , replyFile
  , listen
  , listenToSocket
  , broadcast
  , send
  , Request
  , SocketServer
  , ClientId
  )

import Task exposing (Task, andThen)
import Json.Encode as Encode exposing (Value)
import Process

import Multitier.Server.HttpServer.LowLevel as Http

type alias Request = Http.Request

type alias SocketServer = Http.SocketServer
type alias ClientId = Http.ClientId


-- COMMANDS

type MyCmd msg = Reply Request Value | ReplyFile Request String |
                 Broadcast SocketServer String | Send SocketServer Int String

reply : Request -> Value -> Cmd msg
reply request message =
  command (Reply request message)

replyFile : Request -> String -> Cmd msg
replyFile request filename = command (ReplyFile request filename)

broadcast : SocketServer -> String -> Cmd msg
broadcast server message = command (Broadcast server message)

send : SocketServer -> Int -> String -> Cmd msg
send server cid message = command (Send server cid message)

cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap _ cmd = case cmd of
  Reply request msg -> Reply request msg
  ReplyFile request filename -> ReplyFile request filename
  Broadcast server message -> Broadcast server message
  Send server cid message -> Send server cid message



-- SUBSCRIPTIONS


type MySub msg = Listen Int (Request -> msg) | ListenSocket (SocketServer -> msg) (ClientId -> msg)  (ClientId -> msg) ((ClientId, String) -> msg)

listen : Int -> (Request -> msg) -> Sub msg
listen portNumber tagger =
  subscription (Listen portNumber tagger)

listenToSocket : (SocketServer -> msg) -> (ClientId -> msg) -> (ClientId -> msg) -> ((ClientId,String) -> msg) -> Sub msg
listenToSocket onSocketOpen onConnect onDisconnect onMessage =
  subscription (ListenSocket onSocketOpen onConnect onDisconnect onMessage)

subMap : (a -> b) -> MySub a -> MySub b
subMap func sub = case sub of
  Listen portNumber tagger -> Listen portNumber (tagger >> func)
  ListenSocket onSocketOpen onConnect onDisconnect onMessage -> ListenSocket (onSocketOpen >> func) (onConnect >> func) (onDisconnect >> func) (onMessage >> func)


-- MANAGER

type alias State msg =
  { server: Maybe Http.Server
  , socketServer: Maybe SocketServer
  , httpSub : Maybe (Int, (Request -> msg))
  , socketSubs : List ((SocketServer -> msg), (ClientId -> msg), (ClientId -> msg), ((ClientId,String) -> msg))
  }

init : Task Never (State msg)
init = Task.succeed (State Nothing Nothing Nothing [])



-- HANDLE APP MESSAGES


onEffects : Platform.Router msg Msg -> List (MyCmd msg) -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router cmdList subList state =
  makeSubs router subList { state | socketSubs = [] }
  |> andThen (startHttpServerIfNeeded router)
  |> andThen (handleCommands router cmdList)

makeSubs : Platform.Router msg Msg -> List (MySub msg) -> State msg -> Task Never (State msg)
makeSubs router subList state = case subList of
  Listen portNumber tagger :: subs -> case state.httpSub of
    Nothing -> makeSubs router subs { state | httpSub = Just (portNumber, tagger) }
    _ -> makeSubs router subs state
  ListenSocket onSocketOpen onConnect onDisconnect onMessage :: subs -> makeSubs router subs { state | socketSubs = (onSocketOpen,onConnect,onDisconnect,onMessage) :: state.socketSubs }
  [] -> Task.succeed state

startHttpServerIfNeeded : Platform.Router msg Msg -> State msg -> Task Never (State msg)
startHttpServerIfNeeded router state = case state.server of
  Nothing -> case state.httpSub of
    Just (portNumber, tagger) ->  Platform.sendToSelf router (StartServer portNumber) |> andThen (\_ -> Task.succeed state)
    _ -> Task.succeed state
  _ -> Task.succeed state

handleCommands : Platform.Router msg Msg -> List (MyCmd msg) -> State msg -> Task Never (State msg)
handleCommands router cmdList state = case cmdList of
  [] -> Task.succeed state
  Reply request msg :: cmds -> Http.reply request msg |> andThen (\_ -> handleCommands router cmds state)
  ReplyFile request filename :: cmds -> Http.replyFile request filename |> andThen (\_ -> handleCommands router cmds state)
  Broadcast server message :: cmds -> Http.broadcast server message |> andThen (\_ -> handleCommands router cmds state)
  Send server cid message :: cmds -> Http.send server cid message |> andThen (\_ -> handleCommands router cmds state)


-- HANDLE SELF MESSAGES


type Msg = StartServer Int |
           OnStart Http.Server | OnClose Int |
           OnRequest Request |
           OnMessage ClientId String | OnConnect ClientId | OnDisconnect ClientId


onSelfMsg : Platform.Router msg Msg -> Msg -> State msg -> Task Never (State msg)
onSelfMsg router selfMsg state =
  case selfMsg of
    OnRequest request -> case state.httpSub of
      Just (_, tagger) -> Platform.sendToApp router (tagger request) |> Task.andThen (\_ -> Task.succeed state)
      _ -> Task.succeed state

    OnClose portNumber ->
      Task.succeed state

    OnConnect clientId ->
      case state.socketServer of
        Just socketServer ->
          let messages = List.map (\(_,onConnect,_,_) -> Platform.sendToApp router (onConnect clientId)) state.socketSubs
          in Task.sequence messages
            |> Task.andThen (\_ -> Task.succeed state)
        _ -> Task.succeed state
    OnDisconnect clientId ->
      case state.socketServer of
        Just socketServer ->
          let messages = List.map (\(_,_,onDisconnect,_) -> Platform.sendToApp router (onDisconnect clientId)) state.socketSubs
          in Task.sequence messages
            |> Task.andThen (\_ -> Task.succeed state)
        _ -> Task.succeed state
    OnMessage clientId message ->
      case state.socketServer of
        Just socketServer ->
          let messages = List.map (\(_,_,_,onMessage) -> Platform.sendToApp router (onMessage (clientId,message))) state.socketSubs
          in Task.sequence messages
            |> Task.andThen (\_ -> Task.succeed state)
        _ -> Task.succeed state

    StartServer portNumber -> start router portNumber |> andThen (\_ -> Task.succeed state)
    OnStart server -> openSocketIfNeeded router { state | server = Just server } server
-- START WEBSOCKETSERVER


openSocketIfNeeded : Platform.Router msg Msg -> State msg -> Http.Server -> Task Never (State msg)
openSocketIfNeeded router state server =
  case state.socketSubs of
    tagger :: _ -> Http.openSocket server { onMessage = \message -> Platform.sendToSelf router (OnMessage message.clientId message.data) }
      |> andThen (\socketServer -> Task.sequence (List.map (\(onSocketOpen,_,_,_) -> Platform.sendToApp router (onSocketOpen socketServer)) state.socketSubs)
        |> andThen (\_ -> Task.succeed { state | socketServer = Just socketServer}))
    _ -> Task.succeed state

-- START SERVER

start : Platform.Router msg Msg -> Int -> Task x Process.Id
start router portNumber =
  Process.spawn (Http.listen portNumber
    { onRequest = \request -> Platform.sendToSelf router (OnRequest request)
    , onClose = \_ -> Platform.sendToSelf router (OnClose portNumber)
    } |> andThen (\server -> Platform.sendToSelf router (OnStart server) ))
