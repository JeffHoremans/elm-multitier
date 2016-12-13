module Multitier
  exposing ( MultitierCmd
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
           )

import Html exposing (Html)
import Task exposing (Task, andThen)
import Http
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder, map2, field)
import String
import WebSocket

import HttpServer
import Server.File as File
import HttpServer.Utils exposing (Method(..))
import Multitier.Error exposing (Error(..))
import Multitier.Procedure exposing (..)
import Multitier.LowLevel as LowLevel exposing (fromJSON, toJSON, fromJSONString)

type MultitierCmd procedure msg = ServerCmd procedure | ClientCmd (Cmd msg) | Batch (List (MultitierCmd procedure msg))

performOnServer : procedure -> MultitierCmd procedure msg
performOnServer proc = ServerCmd proc

performOnClient : Cmd msg -> MultitierCmd procedure msg
performOnClient cmd = ClientCmd cmd

map : (a -> procedure) -> (b -> msg) -> MultitierCmd a b -> MultitierCmd procedure msg
map fa fb mtcmd = case mtcmd of
  ServerCmd procedure -> ServerCmd (fa procedure)
  ClientCmd cmd -> ClientCmd (Cmd.map fb cmd)
  Batch cmds    -> Batch (List.map (map fa fb) cmds)

batch : List (MultitierCmd procedure msg) -> MultitierCmd procedure msg
batch = Batch

none : MultitierCmd procedure msg
none = batch []

(!!) : model -> List (MultitierCmd procedure msg) -> (model, MultitierCmd procedure msg)
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

unbatch : Config -> (procedure -> Procedure serverModel msg) -> MultitierCmd procedure msg -> Cmd msg
unbatch config proceduresMap mtcmd =
  case mtcmd of
    ServerCmd procedure ->
      let (Proc handler _) = proceduresMap procedure in
        Http.send (\result -> handler ((Result.mapError (\err -> NetworkError err) result)
                                          |> Result.andThen (\response -> case response.data of
                                              Just data -> Ok data
                                              _ -> Err (ServerError response.message))))
          (Http.post  ("http://" ++ config.hostname ++ ":" ++ (toString config.httpPort) ++ "/procedure" )
                      (Http.jsonBody (toJSON procedure)) decodeResponse)

    ClientCmd cmd -> cmd
    Batch cmds    -> Cmd.batch (List.map (unbatch config proceduresMap) cmds)



type ServerMsg msg = ServerUserMsg msg | Request HttpServer.Request | Reply HttpServer.Request String (Maybe Value) | ReplyFile HttpServer.Request String
type ClientMsg msg = ClientUserMsg msg | Update String

unwrapInit :
     Config
  -> (procedure -> Procedure serverModel msg)
  -> ( model, MultitierCmd procedure msg)
  -> ( model, Cmd msg )
unwrapInit config proceduresMap ( model, cmds) = (model, unbatch config proceduresMap cmds)

unwrapInitWithFlags :
     Config
  -> (procedure -> Procedure serverModel msg)
  -> (input -> ( model, MultitierCmd procedure msg))
  -> (String -> ( model, Cmd msg ))
unwrapInitWithFlags config proceduresMap init =
  \str ->
    let serverState = fromJSONString str in
      let (model, cmds) = init serverState in
        (model, unbatch config proceduresMap cmds)

unwrapUpdate :
     Config
  -> (procedure -> Procedure serverModel msg)
  -> (msg -> model -> ( model, MultitierCmd procedure msg ))
  -> (msg -> model -> ( model, Cmd msg ))
unwrapUpdate config proceduresMap update =
  \msg model ->
    let (newModel, cmds) = update msg model
    in  (newModel, unbatch config proceduresMap cmds)

unwrapStateUpdate :
     Config
  -> (procedure -> Procedure serverModel msg)
  -> (serverState -> model -> ( model, MultitierCmd procedure msg ))
  -> (serverState -> model -> ( model, Cmd msg ))
unwrapStateUpdate config proceduresMap update =
  \msg model ->
    let (newModel, cmds) = update msg model
    in  (newModel, unbatch config proceduresMap cmds)

clientProgram :
       { config: Config
       , init : serverState -> ( model, MultitierCmd proc msg )
       , update : msg -> model -> ( model, MultitierCmd proc msg )
       , stateUpdate: serverState -> model -> ( model, MultitierCmd proc msg)
       , subscriptions : model -> Sub msg
       , view : model -> Html msg
       , initServer: serverModel
       , serverState : serverModel -> serverState
       , procedures : proc -> Procedure serverModel msg
       , updateServer : serverMsg -> serverModel -> (serverModel, Cmd serverMsg)
       , serverSubscriptions : serverModel -> Sub serverMsg
       }
    -> Program String model (ClientMsg msg)
clientProgram stuff =
  let init = unwrapInitWithFlags stuff.config stuff.procedures stuff.init
      update = unwrapUpdate stuff.config stuff.procedures stuff.update
      stateUpdate = unwrapStateUpdate stuff.config stuff.procedures stuff.stateUpdate
  in let wrapUpdate msg model =
          case msg of
            ClientUserMsg userMsg -> updateHelp ClientUserMsg <| update userMsg model
            Update message -> updateHelp ClientUserMsg <| stateUpdate (fromJSONString message) model
         wrapSubscriptions model =
           Sub.batch [ Sub.map ClientUserMsg (stuff.subscriptions model), WebSocket.listen ("ws://" ++ stuff.config.hostname ++ ":" ++ (toString stuff.config.httpPort)) Update]
         wrapInit input =
           let (model, cmds) = init input
           in (model, Cmd.map ClientUserMsg cmds)
         wrapView model = Html.map ClientUserMsg (stuff.view model)
  in Html.programWithFlags
    { init = wrapInit
    , update = wrapUpdate
    , subscriptions = wrapSubscriptions
    , view = wrapView
    }

serverProgram :
       { config: Config
       , init : input -> ( model, MultitierCmd proc msg )
       , update : msg -> model -> ( model, MultitierCmd proc msg )
       , subscriptions : model -> Sub msg
       , view : model -> Html msg
       , initServer: serverModel
       , serverState : serverModel -> input
       , procedures : proc -> Procedure serverModel msg
       , updateServer : serverMsg -> serverModel -> (serverModel, Cmd serverMsg)
       , serverSubscriptions : serverModel -> Sub serverMsg
       }
    -> Program Never serverModel (ServerMsg serverMsg)
serverProgram stuff =
    let update = stuff.updateServer
            in let wrapInit =  (stuff.initServer, Cmd.none)
                   wrapUpdate msg model = let (newModel, cmds) =
                    case msg of
                      ServerUserMsg userMsg -> updateHelp ServerUserMsg <| update userMsg model
                      Request request -> handle stuff.serverState stuff.procedures request model
                      Reply request message data -> (model, HttpServer.reply request (encodeResponse (Response data message)))
                      ReplyFile request file ->  (model, HttpServer.replyFile request file)
                    in case model == newModel of
                      True -> (newModel, cmds)
                      False -> (newModel, Cmd.batch [cmds, HttpServer.broadcast (Encode.encode 0 (toJSON (stuff.serverState newModel))) ])
                   wrapSubscriptions model =
                     Sub.batch [ HttpServer.listen stuff.config.httpPort Request, Sub.map ServerUserMsg (stuff.serverSubscriptions model)]

    in Platform.program
          { init = wrapInit
          , update = wrapUpdate
          , subscriptions = wrapSubscriptions
          }

handle : (serverModel -> input) -> (procedure -> Procedure serverModel msg) -> HttpServer.Request -> serverModel -> (serverModel, Cmd (ServerMsg serverMsg))
handle serverState procedures request model =
  let pathList = List.filter (not << String.isEmpty) (String.split "/" request.path)
      invalidRequest = \message -> (model, HttpServer.reply request (encodeResponse (Response Nothing message)))
  in case request.method of
    GET -> case pathList of
      [] -> (model, Task.attempt (\result -> case result of
                                    Err _ ->    Reply request "Invalid request" Nothing
                                    _     ->    ReplyFile request "examples/index.html")
                                 (File.write "examples/state.js" ("let state="++ (toString (Encode.encode 0 (toJSON (serverState model)))))))
      ["client.js"] -> (model, HttpServer.replyFile request "examples/client.js")
      ["state.js"] -> (model, HttpServer.replyFile request "examples/state.js")
      _ -> invalidRequest "Invalid request"
    POST -> case pathList of
      ["procedure"] -> let (Proc _ toTask) = procedures (fromJSONString request.body) in
        let (newModel, task) = toTask model in
          (newModel, Task.attempt (\result -> case result of
                                    Err _ ->      Reply request "Procedure failed" Nothing
                                    Ok value ->   Reply request "" (Just value)) task)
      _ -> invalidRequest "Invalid request"
    _ -> invalidRequest "Invalid request"

updateHelp : (a -> b) -> (model, Cmd a) -> (model, Cmd b)
updateHelp func (model, cmds) =
  (model, Cmd.map func cmds)
