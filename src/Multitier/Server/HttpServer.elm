effect module Multitier.Server.HttpServer where { command = MyCmd, subscription = MySub } exposing
  ( reply
  , replyFile
  , listen
  , listenToSocket
  , broadcast
  , send
  , Request
  , ClientId
  , encodecid
  , decodecid
  )

import Task exposing (Task, andThen)
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Process
import Dict exposing (Dict)
import Set exposing (Set)
import Array exposing (Array)

import Multitier.Server.HttpServer.LowLevel as Http

type alias Request = Http.Request

type alias SocketRouter = Http.SocketRouter
type alias ClientId = Http.ClientId

encodecid : ClientId -> Value
encodecid = Http.encodecid

decodecid : Decoder ClientId
decodecid = Http.decodecid

-- COMMANDS

type MyCmd msg = Reply Request Value | ReplyFile Request String |
                 Broadcast String String | Send String ClientId String

reply : Request -> Value -> Cmd msg
reply request message =
  command (Reply request message)

replyFile : Request -> String -> Cmd msg
replyFile request filename = command (ReplyFile request filename)

broadcast : String -> String -> Cmd msg
broadcast path message = command (Broadcast path message)

send : String -> ClientId -> String -> Cmd msg
send path cid message = command (Send path cid message)

cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap _ cmd = case cmd of
  Reply request msg -> Reply request msg
  ReplyFile request filename -> ReplyFile request filename
  Broadcast server message -> Broadcast server message
  Send server cid message -> Send server cid message



-- SUBSCRIPTIONS


type MySub msg = Listen Int (Request -> msg) | ListenSocket String (Maybe (ClientId -> msg))  (Maybe (ClientId -> msg)) (Maybe ((ClientId, String) -> msg))

listen : Int -> (Request -> msg) -> Sub msg
listen portNumber tagger =
  subscription (Listen portNumber tagger)

listenToSocket : String  -> Maybe (ClientId -> msg) -> Maybe (ClientId -> msg) -> Maybe ((ClientId,String) -> msg) -> Sub msg
listenToSocket path onConnect onDisconnect onMessage =
  subscription (ListenSocket path onConnect onDisconnect onMessage)

subMap : (a -> b) -> MySub a -> MySub b
subMap func sub = case sub of
  Listen portNumber tagger -> Listen portNumber (tagger >> func)
  ListenSocket path onConnect onDisconnect onMessage ->
    let map = \may -> case may of
      Just onFunc -> Just (onFunc >> func)
      _ -> Nothing in
      ListenSocket path (map onConnect) (map onDisconnect) (map onMessage)


-- MANAGER

type alias State msg =
  { server: Maybe Http.Server
  , socketRouter: Maybe SocketRouter
  , httpSub : Maybe (Int, (Request -> msg))
  , socketSubs : Dict String (Array (Maybe (ClientId -> msg), Maybe (ClientId -> msg), Maybe ((ClientId,String) -> msg)))
  , mounted : Set String
  }

init : Task Never (State msg)
init = Task.succeed (State Nothing Nothing Nothing Dict.empty Set.empty)

(&>) : Task x a -> Task x b -> Task x b
(&>) t1 t2 =
  Task.andThen (\_ -> t2) t1

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
  ListenSocket path onConnect onDisconnect onMessage :: subs ->
    makeSubs router subs { state | socketSubs = Dict.update path
      (\v -> case v of
        Just value -> Just (Array.push (onConnect,onDisconnect,onMessage) value)
        _ -> Just (Array.push (onConnect,onDisconnect,onMessage) Array.empty)) state.socketSubs }
  [] -> Task.succeed state

startHttpServerIfNeeded : Platform.Router msg Msg -> State msg -> Task Never (State msg)
startHttpServerIfNeeded router state = case state.server of
  Nothing -> case state.httpSub of
    Just (portNumber, tagger) ->  Platform.sendToSelf router (StartServer portNumber) &> Task.succeed state
    _ -> Task.succeed state
  _ -> Task.succeed state

checkSockets : Platform.Router msg Msg -> State msg -> Task Never (State msg)
checkSockets router state = case state.socketRouter of
  Just socketRouter ->
    let openSockets = state.socketSubs
      |> Dict.keys
      |> List.map (\path -> case Set.member path state.mounted of
        False -> Http.openSocket socketRouter path { onMessage = \message -> Platform.sendToSelf router (OnMessage message.clientId message.data)
                                                    , onConnect = \cid -> Platform.sendToSelf router (OnConnect cid)
                                                    , onDisconnect = \cid -> Platform.sendToSelf router (OnDisconnect cid)}
                  &> Task.succeed (path)
        True -> Task.succeed (path))
    in Task.sequence openSockets
      |> andThen (\paths ->
        let newMounted = Set.fromList paths in
          let toUnmount = Set.diff state.mounted newMounted
            |> Set.toList
            |> List.map (\path -> Http.closeSocket socketRouter path)
          in Task.sequence toUnmount
              &>  Task.succeed { state | mounted = newMounted })
  _ -> Task.succeed state

handleCommands : Platform.Router msg Msg -> List (MyCmd msg) -> State msg -> Task Never (State msg)
handleCommands router cmdList state = case cmdList of
  [] -> Task.succeed state
  Reply request msg :: cmds -> Http.reply request msg &> handleCommands router cmds state
  ReplyFile request filename :: cmds -> Http.replyFile request filename &> handleCommands router cmds state
  Broadcast path message :: cmds -> Http.broadcast path message &> handleCommands router cmds state
  Send path cid message :: cmds -> Http.send path cid message &> handleCommands router cmds state


-- HANDLE SELF MESSAGES


type Msg = StartServer Int |
           OnStart Http.Server | OnClose Int |
           OnRequest Request |
           OnMessage ClientId String | OnConnect ClientId | OnDisconnect ClientId


onSelfMsg : Platform.Router msg Msg -> Msg -> State msg -> Task Never (State msg)
onSelfMsg router selfMsg state =
  case selfMsg of
    OnRequest request -> case state.httpSub of
      Just (_, tagger) -> Platform.sendToApp router (tagger request) &> Task.succeed state
      _ -> Task.succeed state

    OnClose portNumber ->
      Task.succeed state

    OnConnect (Http.ClientId path clientId) ->
      case state.socketRouter of
        Just socketRouter -> case Dict.get path state.socketSubs of
          Just subs -> subs
            |> Array.map (\(maybeOnConnect,_,_) -> case maybeOnConnect of
              Just onConnect -> Platform.sendToApp router (onConnect (Http.ClientId path clientId))
              _ -> Task.succeed ())
            |> Array.toList
            |> Task.sequence
            |> Task.andThen (\_ -> Task.succeed state)
          _ -> Task.succeed state
        _ -> Task.succeed state
    OnDisconnect (Http.ClientId path clientId) ->
      case state.socketRouter of
        Just socketRouter -> case Dict.get path state.socketSubs of
          Just subs -> subs
            |> Array.map (\(_,maybeOnDisconnect,_) -> case maybeOnDisconnect of
              Just onDisconnect -> Platform.sendToApp router (onDisconnect (Http.ClientId path clientId))
              _ -> Task.succeed ())
            |> Array.toList
            |> Task.sequence
            |> Task.andThen (\_ -> Task.succeed state)
          _ -> Task.succeed state
        _ -> Task.succeed state
    OnMessage (Http.ClientId path clientId) message ->
      case state.socketRouter of
        Just socketRouter -> case Dict.get path state.socketSubs of
          Just subs -> subs
            |> Array.map (\(_,_,maybeOnMessage) -> case maybeOnMessage of
              Just onMessage -> Platform.sendToApp router (onMessage ((Http.ClientId path clientId),message))
              _ -> Task.succeed ())
            |> Array.toList
            |> Task.sequence
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
