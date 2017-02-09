effect module Multitier.Server.HttpServer where { command = MyCmd, subscription = MySub } exposing
  ( reply
  , replyFile
  , listen
  , listenToSocket
  , broadcast
  , send
  , Request
  , Socket
  , ClientId
  , encodecid
  , decodecid
  )

import Task exposing (Task, andThen)
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Process
import Dict exposing (Dict)

import Multitier.Server.HttpServer.LowLevel as Http

type alias Request = Http.Request

type alias SocketRouter = Http.SocketRouter
type alias Socket = Http.Socket
type alias ClientId = Http.ClientId

encodecid : ClientId -> Value
encodecid = Http.encodecid

decodecid : Decoder ClientId
decodecid = Http.decodecid

-- COMMANDS

type MyCmd msg = Reply Request Value | ReplyFile Request String |
                 Broadcast Socket String | Send Socket ClientId String

reply : Request -> Value -> Cmd msg
reply request message =
  command (Reply request message)

replyFile : Request -> String -> Cmd msg
replyFile request filename = command (ReplyFile request filename)

broadcast : Socket -> String -> Cmd msg
broadcast server message = command (Broadcast server message)

send : Socket -> ClientId -> String -> Cmd msg
send server cid message = command (Send server cid message)

cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap _ cmd = case cmd of
  Reply request msg -> Reply request msg
  ReplyFile request filename -> ReplyFile request filename
  Broadcast server message -> Broadcast server message
  Send server cid message -> Send server cid message



-- SUBSCRIPTIONS


type MySub msg = Listen Int (Request -> msg) | ListenSocket String (Socket -> msg) (ClientId -> msg)  (ClientId -> msg) ((ClientId, String) -> msg)

listen : Int -> (Request -> msg) -> Sub msg
listen portNumber tagger =
  subscription (Listen portNumber tagger)

listenToSocket : String -> (Socket -> msg) -> (ClientId -> msg) -> (ClientId -> msg) -> ((ClientId,String) -> msg) -> Sub msg
listenToSocket path onSocketOpen onConnect onDisconnect onMessage =
  subscription (ListenSocket path onSocketOpen onConnect onDisconnect onMessage)

subMap : (a -> b) -> MySub a -> MySub b
subMap func sub = case sub of
  Listen portNumber tagger -> Listen portNumber (tagger >> func)
  ListenSocket path onSocketOpen onConnect onDisconnect onMessage -> ListenSocket path (onSocketOpen >> func) (onConnect >> func) (onDisconnect >> func) (onMessage >> func)


-- MANAGER

type alias State msg =
  { server: Maybe Http.Server
  , socketRouter: Maybe SocketRouter
  , httpSub : Maybe (Int, (Request -> msg))
  , socketSubs : Dict String ((Socket -> msg), (ClientId -> msg), (ClientId -> msg), ((ClientId,String) -> msg))
  , mounted : Dict String Socket
  }

init : Task Never (State msg)
init = Task.succeed (State Nothing Nothing Nothing Dict.empty Dict.empty)



-- HANDLE APP MESSAGES


onEffects : Platform.Router msg Msg -> List (MyCmd msg) -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router cmdList subList state =
  makeSubs router subList { state | socketSubs = Dict.empty }
  |> andThen (startHttpServerIfNeeded router)
  |> andThen (checkSockets router)
  |> andThen (handleCommands router cmdList)

makeSubs : Platform.Router msg Msg -> List (MySub msg) -> State msg -> Task Never (State msg)
makeSubs router subList state = case subList of
  Listen portNumber tagger :: subs -> case state.httpSub of
    Nothing -> makeSubs router subs { state | httpSub = Just (portNumber, tagger) }
    _ -> makeSubs router subs state
  ListenSocket path onSocketOpen onConnect onDisconnect onMessage :: subs -> makeSubs router subs { state | socketSubs = Dict.insert path (onSocketOpen,onConnect,onDisconnect,onMessage) state.socketSubs }
  [] -> Task.succeed state

startHttpServerIfNeeded : Platform.Router msg Msg -> State msg -> Task Never (State msg)
startHttpServerIfNeeded router state = case state.server of
  Nothing -> case state.httpSub of
    Just (portNumber, tagger) ->  Platform.sendToSelf router (StartServer portNumber) |> andThen (\_ -> Task.succeed state)
    _ -> Task.succeed state
  _ -> Task.succeed state

checkSockets : Platform.Router msg Msg -> State msg -> Task Never (State msg)
checkSockets router state = case state.socketRouter of
  Just socketRouter ->
    let openSockets = state.socketSubs
      |> Dict.toList
      |> List.map (\(path, (onSocketOpen,onConnect,onDisconnect,onMessage)) -> case Dict.get path state.mounted of
        Nothing -> Http.openSocket socketRouter path { onMessage = \message -> Platform.sendToSelf router (OnMessage message.clientId message.data)
                                                    , onConnect = \cid -> Platform.sendToSelf router (OnConnect cid)
                                                    , onDisconnect = \cid -> Platform.sendToSelf router (OnDisconnect cid)}
                  |> andThen (\socket -> Platform.sendToApp router (onSocketOpen socket) |> andThen (\_ -> Task.succeed ((path,socket))))
        Just socket -> Task.succeed (path,socket))
    in Task.sequence openSockets
      |> andThen (\sockets ->
        let newMounted = Dict.fromList sockets in
          let toUnmount = Dict.diff state.mounted newMounted
            |> Dict.toList
            |> List.map (\(path, socket) -> Http.closeSocket socketRouter socket)
          in Task.sequence toUnmount
              |> andThen (\_ -> Task.succeed { state | mounted = newMounted }))
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

    OnConnect (Http.ClientId path clientId) ->
      case state.socketRouter of
        Just socketRouter -> case Dict.get path state.socketSubs of
          Just (_,onConnect,_,_) -> Platform.sendToApp router (onConnect (Http.ClientId path clientId))
            |> Task.andThen (\_ -> Task.succeed state)
          _ -> Task.succeed state
        _ -> Task.succeed state
    OnDisconnect (Http.ClientId path clientId) ->
      case state.socketRouter of
        Just socketRouter -> case Dict.get path state.socketSubs of
          Just (_,_,onDisconnect,_) ->  Platform.sendToApp router (onDisconnect (Http.ClientId path clientId))
            |> Task.andThen (\_ -> Task.succeed state)
          _ -> Task.succeed state
        _ -> Task.succeed state
    OnMessage (Http.ClientId path clientId) message ->
      case state.socketRouter of
        Just socketRouter -> case Dict.get path state.socketSubs of
          Just (_,_,_,onMessage) -> Platform.sendToApp router (onMessage ((Http.ClientId path clientId),message))
            |> Task.andThen (\_ -> Task.succeed state)
          _ -> Task.succeed state
        _ -> Task.succeed state

    StartServer portNumber -> start router portNumber |> andThen (\_ -> Task.succeed state)
    OnStart server ->
      createSocketRouter router { state | server = Just server } server
        |> andThen (checkSockets router)

-- START WEBSOCKETSERVER

createSocketRouter : Platform.Router msg Msg -> State msg -> Http.Server -> Task Never (State msg)
createSocketRouter router state server =
  Http.createSocketRouter server
    |> andThen (\socketRouter -> Task.succeed { state | socketRouter = Just socketRouter })


-- START SERVER

start : Platform.Router msg Msg -> Int -> Task x Process.Id
start router portNumber =
  Process.spawn (Http.listen portNumber
    { onRequest = \request -> Platform.sendToSelf router (OnRequest request)
    , onClose = \_ -> Platform.sendToSelf router (OnClose portNumber)
    } |> andThen (\server -> Platform.sendToSelf router (OnStart server) ))
