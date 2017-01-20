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
import Multitier.Procedure exposing (..)
import Multitier.Server.File as File
import Multitier.LowLevel as LowLevel exposing (fromJSON, toJSON, fromJSONString)

type MultitierCmd procedure msg = ServerCmd procedure |
                                  ClientCmd (Cmd msg) |

                                  Batch (List (MultitierCmd procedure msg))

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

type ServerMsg msg = ServerUserMsg msg | Request HttpServer.Request | Reply HttpServer.Request String (Maybe Value) | ReplyFile HttpServer.Request String
type ClientMsg msg = ClientUserMsg msg

type alias MultitierProgram model serverModel msg serverMsg =
  { client : Program String model (ClientMsg msg)
  , server : Program Never serverModel (ServerMsg serverMsg)
  }

program :
  { config: Config
  , init : serverState -> ( model, MultitierCmd proc msg )
  , update : msg -> model -> ( model, MultitierCmd proc msg )
  , subscriptions : model -> Sub msg
  , view : model -> Html msg
  , serverState : serverModel -> serverState
  , procedures : proc -> Procedure serverModel msg serverMsg
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
       , init : serverState -> ( model, MultitierCmd proc msg )
       , update : msg -> model -> ( model, MultitierCmd proc msg )
       , subscriptions : model -> Sub msg
       , view : model -> Html msg
       , serverState : serverModel -> serverState
       , procedures : proc -> Procedure serverModel msg serverMsg
       , initServer: (serverModel, Cmd serverMsg)
       , updateServer : serverMsg -> serverModel -> (serverModel, Cmd serverMsg)
       , serverSubscriptions : serverModel -> Sub serverMsg
       }
    -> { init : String -> (model, Cmd (ClientMsg msg))
       , update : (ClientMsg msg) -> model -> (model, Cmd (ClientMsg msg))
       , subscriptions : model -> Sub (ClientMsg msg)
       , view : model -> Html (ClientMsg msg)}
clientStuff stuff =
  let init = unwrapInitWithFlags stuff.config stuff.procedures stuff.init
      update = unwrapUpdate stuff.config stuff.procedures stuff.update
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
       , init : serverState -> ( model, MultitierCmd proc msg )
       , update : msg -> model -> ( model, MultitierCmd proc msg )
       , subscriptions : model -> Sub msg
       , view : model -> Html msg
       , serverState : serverModel -> serverState
       , procedures : proc -> Procedure serverModel msg serverMsg
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
            Request request -> handle stuff.serverState stuff.procedures request model
            Reply request message data -> (model, HttpServer.reply request (encodeResponse (Response data message)))
            ReplyFile request file ->  (model, HttpServer.replyFile request file)
        wrapSubscriptions model =
          Sub.batch [ HttpServer.listen stuff.config.httpPort Request, Sub.map ServerUserMsg (stuff.serverSubscriptions model)]

    in { init = wrapInit
       , update = wrapUpdate
       , subscriptions = wrapSubscriptions
       }

unbatch : Config -> (procedure -> Procedure serverModel msg serverMsg) -> MultitierCmd procedure msg -> Cmd msg
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

unwrapInit :
     Config
  -> (procedure -> Procedure serverModel msg serverMsg)
  -> ( model, MultitierCmd procedure msg)
  -> ( model, Cmd msg )
unwrapInit config proceduresMap ( model, cmds) = (model, unbatch config proceduresMap cmds)

unwrapInitWithFlags :
     Config
  -> (procedure -> Procedure serverModel msg serverMsg)
  -> (input -> ( model, MultitierCmd procedure msg))
  -> (String -> ( model, Cmd msg ))
unwrapInitWithFlags config proceduresMap init =
  \str ->
    let serverState = fromJSONString str in
      let (model, cmds) = init serverState in
        (model, unbatch config proceduresMap cmds)

unwrapUpdate :
     Config
  -> (procedure -> Procedure serverModel msg serverMsg)
  -> (msg -> model -> ( model, MultitierCmd procedure msg ))
  -> (msg -> model -> ( model, Cmd msg ))
unwrapUpdate config proceduresMap update =
  \msg model ->
    let (newModel, cmds) = update msg model
    in  (newModel, unbatch config proceduresMap cmds)


handle : (serverModel -> input) -> (procedure -> Procedure serverModel msg serverMsg) -> HttpServer.Request -> serverModel -> (serverModel, Cmd (ServerMsg serverMsg))
handle serverState procedures request model =
  let pathList = List.filter (not << String.isEmpty) (String.split "/" request.path)
      invalidRequest = \message -> (model, HttpServer.reply request (encodeResponse (Response Nothing message)))
  in case request.method of
    GET -> case pathList of
      [] -> (model, Task.attempt (\result -> case result of
                                    Err _ ->    Reply request "Invalid request" Nothing
                                    _     ->    ReplyFile request "index.html")
                                 (File.write "examples/state.js" ("let state="++ (toString (Encode.encode 0 (toJSON (serverState model)))))))
      [filename] -> case File.exists filename of
        True -> (model, HttpServer.replyFile request ("examples/" ++  filename))
        _ -> invalidRequest "File not found."
      _ -> invalidRequest "Invalid request"
    POST -> case pathList of
      ["procedure"] -> let (Proc _ update) = procedures (fromJSONString request.body) in
        let (newModel, task, cmd) = update model in
          (newModel, Cmd.batch [ Cmd.map ServerUserMsg cmd, Task.attempt (\result -> case result of
                                      Err _ ->      Reply request "Procedure failed" Nothing
                                      Ok value ->   Reply request "" (Just value)) task])
      _ -> invalidRequest "Invalid request"
    _ -> invalidRequest "Invalid request"

updateHelp : (a -> b) -> (model, Cmd a) -> (model, Cmd b)
updateHelp func (model, cmds) =
  (model, Cmd.map func cmds)
