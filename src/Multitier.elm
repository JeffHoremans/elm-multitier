module Multitier
  exposing ( MultitierCmd(..)
           , map
           , batch
           , none
           , (!!)

           , performOnClient
           , performOnServer

           , Config
           , ServerMsg, ClientMsg
           , clientProgram
           , serverProgram
           , MultitierProgram
           , program
           )

import Html exposing (Html)
import Task exposing (Task, andThen)
import Http
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder, map2, field)
import String

import Multitier.Server.HttpServer as HttpServer
import Multitier.Server.HttpServer.Utils exposing (Method(..))
import Multitier.Error exposing (Error(..))
import Multitier.RPC exposing (..)
import Multitier.Server.File as File
import Multitier.LowLevel as LowLevel exposing (fromJSON, toJSON, fromJSONString)

type MultitierCmd remoteServerMsg msg = ServerCmd remoteServerMsg |
                                  ClientCmd (Cmd msg) |

                                  Batch (List (MultitierCmd remoteServerMsg msg))

performOnServer : remoteServerMsg -> MultitierCmd remoteServerMsg msg
performOnServer msg = ServerCmd msg

performOnClient : Cmd msg -> MultitierCmd procedure msg
performOnClient cmd = ClientCmd cmd

map : (a -> remoteServerMsg) -> (b -> msg) -> MultitierCmd a b -> MultitierCmd remoteServerMsg msg
map fa fb mtcmd = case mtcmd of
  ServerCmd remoteServerMsg -> ServerCmd (fa remoteServerMsg)
  ClientCmd cmd -> ClientCmd (Cmd.map fb cmd)
  Batch cmds    -> Batch (List.map (map fa fb) cmds)

batch : List (MultitierCmd remoteServerMsg msg) -> MultitierCmd remoteServerMsg msg
batch = Batch

none : MultitierCmd remoteServerMsg msg
none = batch []

(!!) : model -> List (MultitierCmd remoteServerMsg msg) -> (model, MultitierCmd remoteServerMsg msg)
(!!) model commands = (model, batch commands)

-- PROGRAM

type Status = Success | Error

type alias Response = { --status: Status,
                        data: Maybe Value
                      , message: String }

decodeResponse : Decoder Response
decodeResponse = map2 Response
  (field "data" (Decode.oneOf [ Decode.null Nothing, Decode.map Just Decode.value ]))
  (field "message" Decode.string)

encodeResponse : Response -> Value
encodeResponse response = Encode.object
  [ ("data", (case response.data of
                Just data -> data
                _ -> Encode.null))
  , ("message", Encode.string response.message) ]



type alias Config = { httpPort: Int
                    , hostname: String }

type ServerMsg msg = ServerUserMsg msg | Request HttpServer.Request | Reply HttpServer.Request String (Maybe Value) | ReplyFile HttpServer.Request String
type ClientMsg msg = ClientUserMsg msg

type alias MultitierProgram model serverModel msg serverMsg =
  { client : Program String model (ClientMsg msg)
  , server : Program Never serverModel (ServerMsg serverMsg)
  }

program :
  { config: Config
  , init : serverState -> ( model, MultitierCmd remoteServerMsg msg )
  , update : msg -> model -> ( model, MultitierCmd remoteServerMsg msg )
  , subscriptions : model -> Sub msg
  , view : model -> Html msg
  , serverState : serverModel -> serverState
  , serverRPCs : remoteServerMsg -> RPC serverModel msg serverMsg
  , initServer: (serverModel, Cmd serverMsg)
  , updateServer : serverMsg -> serverModel -> (serverModel, Cmd serverMsg)
  , serverSubscriptions : serverModel -> Sub serverMsg
  }
  -> MultitierProgram model serverModel msg serverMsg
program stuff =
  let client = Html.programWithFlags (clientStuff stuff)
      server = Platform.program (serverStuff stuff)
  in MultitierProgram client server

clientProgram : MultitierProgram model serverModel msg serverMsg -> Program String model (ClientMsg msg)
clientProgram program = program.client

serverProgram : MultitierProgram model serverModel msg serverMsg -> Program Never serverModel (ServerMsg serverMsg)
serverProgram program = program.server

clientStuff :
       { config: Config
       , init : serverState -> ( model, MultitierCmd remoteServerMsg msg )
       , update : msg -> model -> ( model, MultitierCmd remoteServerMsg msg )
       , subscriptions : model -> Sub msg
       , view : model -> Html msg
       , serverState : serverModel -> serverState
       , serverRPCs : remoteServerMsg -> RPC serverModel msg serverMsg
       , initServer: (serverModel, Cmd serverMsg)
       , updateServer : serverMsg -> serverModel -> (serverModel, Cmd serverMsg)
       , serverSubscriptions : serverModel -> Sub serverMsg
       }
    -> { init : String -> (model, Cmd (ClientMsg msg))
       , update : (ClientMsg msg) -> model -> (model, Cmd (ClientMsg msg))
       , subscriptions : model -> Sub (ClientMsg msg)
       , view : model -> Html (ClientMsg msg)}
clientStuff stuff =
  let init = unwrapInitWithFlags stuff.config stuff.serverRPCs stuff.init
      update = unwrapUpdate stuff.config stuff.serverRPCs stuff.update
  in let wrapUpdate msg model =
          case msg of
            ClientUserMsg userMsg -> updateHelp ClientUserMsg <| update userMsg model
         wrapSubscriptions model =
           Sub.batch [ Sub.map ClientUserMsg (stuff.subscriptions model)]
         wrapInit input =
           let (model, cmds) = init input
           in (model, Cmd.map ClientUserMsg cmds)
         wrapView model = Html.map ClientUserMsg (stuff.view model)
  in { init = wrapInit
     , update = wrapUpdate
     , subscriptions = wrapSubscriptions
     , view = wrapView
     }

serverStuff :
       { config: Config
       , init : serverState -> ( model, MultitierCmd remoteServerMsg msg )
       , update : msg -> model -> ( model, MultitierCmd remoteServerMsg msg )
       , subscriptions : model -> Sub msg
       , view : model -> Html msg
       , serverState : serverModel -> serverState
       , serverRPCs : remoteServerMsg -> RPC serverModel msg serverMsg
       , initServer: (serverModel, Cmd serverMsg)
       , updateServer : serverMsg -> serverModel -> (serverModel, Cmd serverMsg)
       , serverSubscriptions : serverModel -> Sub serverMsg
       }
    -> { init : (serverModel, Cmd (ServerMsg serverMsg))
       , update : (ServerMsg serverMsg) -> serverModel -> (serverModel, Cmd (ServerMsg serverMsg))
       , subscriptions : serverModel -> Sub (ServerMsg serverMsg)
       }
serverStuff stuff =
    let update = stuff.updateServer in
    let wrapInit = let (model, cmds) = stuff.initServer in (model, Cmd.map ServerUserMsg cmds)
        wrapUpdate msg model =
          case msg of
            ServerUserMsg userMsg -> updateHelp ServerUserMsg <| update userMsg model
            Request request -> handle stuff.serverState stuff.serverRPCs request model
            Reply request message data -> (model, HttpServer.reply request (encodeResponse (Response data message)))
            ReplyFile request file ->  (model, HttpServer.replyFile request file)
        wrapSubscriptions model =
          Sub.batch [ HttpServer.listen stuff.config.httpPort Request, Sub.map ServerUserMsg (stuff.serverSubscriptions model)]

    in { init = wrapInit
       , update = wrapUpdate
       , subscriptions = wrapSubscriptions
       }

unbatch : Config -> (remoteServerMsg -> RPC serverModel msg serverMsg) -> MultitierCmd remoteServerMsg msg -> Cmd msg
unbatch config serverRPCs mtcmd =
  case mtcmd of
    ServerCmd remoteServerMsg ->
      let (Rpc handler _) = serverRPCs remoteServerMsg in
        Http.send (\result -> handler ((Result.mapError (\err -> NetworkError err) result)
                                          |> Result.andThen (\response -> case response.data of
                                              Just data -> Ok data
                                              _ -> Err (ServerError response.message))))
          (Http.post  ("http://" ++ config.hostname ++ ":" ++ (toString config.httpPort) ++ "/rpc" )
                      (Http.jsonBody (toJSON remoteServerMsg)) decodeResponse)

    ClientCmd cmd -> cmd
    Batch cmds    -> Cmd.batch (List.map (unbatch config serverRPCs) cmds)

unwrapInit :
     Config
  -> (remoteServerMsg -> RPC serverModel msg serverMsg)
  -> ( model, MultitierCmd remoteServerMsg msg)
  -> ( model, Cmd msg )
unwrapInit config serverRPCs ( model, cmds) = (model, unbatch config serverRPCs cmds)

unwrapInitWithFlags :
     Config
  -> (remoteServerMsg -> RPC serverModel msg serverMsg)
  -> (input -> ( model, MultitierCmd remoteServerMsg msg))
  -> (String -> ( model, Cmd msg ))
unwrapInitWithFlags config serverRPCs init =
  \str ->
    let serverState = fromJSONString str in
      let (model, cmds) = init serverState in
        (model, unbatch config serverRPCs cmds)

unwrapUpdate :
     Config
  -> (remoteServerMsg -> RPC serverModel msg serverMsg)
  -> (msg -> model -> ( model, MultitierCmd remoteServerMsg msg ))
  -> (msg -> model -> ( model, Cmd msg ))
unwrapUpdate config serverRPCs update =
  \msg model ->
    let (newModel, cmds) = update msg model
    in  (newModel, unbatch config serverRPCs cmds)


handle : (serverModel -> serverState) -> (remoteServerMsg -> RPC serverModel msg serverMsg) -> HttpServer.Request -> serverModel -> (serverModel, Cmd (ServerMsg serverMsg))
handle serverState serverRPCs request model =
  let pathList = List.filter (not << String.isEmpty) (String.split "/" request.path)
      invalidRequest = \message -> (model, HttpServer.reply request (encodeResponse (Response Nothing message)))
  in case request.method of
    GET -> case pathList of
      [] -> let state = serverState model in
        ( model , Cmd.batch
          [ Task.attempt (\result -> case result of
              Err _ ->    Reply request "Invalid request" Nothing
              _     ->    ReplyFile request "index.html")
              (File.write "state.js" ("let state="++ (toString (Encode.encode 0 (toJSON state)))))])
      [filename] -> case File.exists filename of
        True -> (model, HttpServer.replyFile request filename)
        _ -> invalidRequest "File not found."
      _ -> invalidRequest "Invalid request"
    POST -> case pathList of
      ["rpc"] -> let (Rpc _ update) = serverRPCs (fromJSONString request.body) in
        let (newModel, task, cmd) = update model in
          (newModel, Cmd.batch [ Cmd.map ServerUserMsg cmd, Task.attempt (\result -> case result of
                                      Err _ ->      Reply request "Procedure failed" Nothing
                                      Ok value ->   Reply request "" (Just value)) task])
      _ -> invalidRequest "Invalid request"
    _ -> invalidRequest "Invalid request"

updateHelp : (a -> b) -> (model, Cmd a) -> (model, Cmd b)
updateHelp func (model, cmds) =
  (model, Cmd.map func cmds)
