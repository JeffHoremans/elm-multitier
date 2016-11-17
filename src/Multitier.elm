module Multitier
  exposing ( MultitierCmd
           , map
           , batch
           , none
           , (!!)

           , performOnClient
           , performOnServer

           , ProgramType(..)
           , Config
           , programWithFlags
           , program
           )

import Html.App as App
import Html exposing (Html)
import Task exposing (Task, andThen)
import Http
import Exts.Http
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder, object2, (:=))
import String
import Worker

import Native.Multitier

import HttpServer
import HttpServer.Utils exposing (Method(..))
import Multitier.Error exposing (Error(..))
import Multitier.Procedure exposing (..)

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
decodeResponse = object2 Response
  ("data"     := Decode.oneOf [ Decode.null Nothing, Decode.map Just Decode.value ])
  ("message"  := Decode.string)

encodeResponse : Response -> Value
encodeResponse response = Encode.object
  [ ("data", (case response.data of
                Just data -> data
                _ -> Encode.null))
  , ("message", Encode.string response.message) ]



type alias Config = { httpPort: Int
                    , hostname: String
                    , clientFile: Maybe String }

unbatch : Config -> (procedure -> RemoteProcedure serverModel msg) -> MultitierCmd procedure msg -> Cmd msg
unbatch config proceduresMap mtcmd =
  case mtcmd of
    ServerCmd procedure ->
      let (RP (onError,onSucceed) _) = proceduresMap procedure in
        Task.perform onError onSucceed
          (((Exts.Http.postJson decodeResponse
              ("http://" ++ config.hostname ++ ":" ++ (toString config.httpPort) ++ "/procedure" )
              (Http.string (Encode.encode 4 (Native.Multitier.toJSON procedure))))

                |> Task.mapError (\err -> NetworkError err))

                  `andThen` \response -> case response.data of
                    Just data -> Task.succeed data
                    _ -> Task.fail (ServerError response.message))

    ClientCmd cmd -> cmd
    Batch cmds    -> Cmd.batch (List.map (unbatch config proceduresMap) cmds)



type MyMsg msg = UserMsg msg | Request HttpServer.Request | Reply HttpServer.Request String (Maybe Value)

unwrapInit :
     Config
  -> (procedure -> RemoteProcedure serverModel msg)
  -> ( model, MultitierCmd procedure msg)
  -> ( model, Cmd msg )
unwrapInit config proceduresMap ( model, cmds) = (model, unbatch config proceduresMap cmds)

unwrapInitWithFlags :
     Config
  -> (procedure -> RemoteProcedure serverModel msg)
  -> ( flags -> ( model, MultitierCmd procedure msg ))
  -> ( flags -> ( model, Cmd msg ))
unwrapInitWithFlags config proceduresMap init =
  \flags ->
    let ( model, cmds) = init flags
    in  ( model, unbatch config proceduresMap cmds)

unwrapUpdate :
     Config
  -> (procedure -> RemoteProcedure serverModel msg)
  -> (msg -> model -> ( model, MultitierCmd procedure msg ))
  -> (msg -> model -> ( model, Cmd msg ))
unwrapUpdate config proceduresMap update =
  \msg model ->
    let (newModel, cmds) = update msg model
    in  (newModel, unbatch config proceduresMap cmds)

type ProgramType = OnServer | OnClient

programWithFlags :
       ProgramType
    -> { config: Config
       , initServer: serverModel
       , procedures : procedure -> RemoteProcedure serverModel msg
       , updateServer : serverMsg -> serverModel -> (serverModel, Cmd serverMsg)
       , serverSubscriptions : serverModel -> Sub serverMsg
       , init : flags -> ( model, MultitierCmd procedure msg )
       , update : msg -> model -> ( model, MultitierCmd procedure msg )
       , subscriptions : model -> Sub msg
       , view : model -> Html msg
       }
    -> Program flags
programWithFlags ptype stuff = case ptype of
  OnClient -> App.programWithFlags
        { init = unwrapInitWithFlags stuff.config stuff.procedures stuff.init
        , update = unwrapUpdate stuff.config stuff.procedures stuff.update
        , subscriptions = stuff.subscriptions
        , view = stuff.view
        }
  OnServer -> let update = stuff.updateServer
            in let wrapInit =  \_ -> (stuff.initServer, Cmd.none)
                   wrapUpdate msg model =
                    case msg of
                      UserMsg userMsg -> updateHelp UserMsg <| update userMsg model
                      Request request -> handle stuff.config stuff.procedures request model
                      Reply request message data -> updateHelp UserMsg <| (model, HttpServer.reply request (encodeResponse (Response data message)))
                   wrapSubscriptions model =
                     Sub.batch [ HttpServer.listen stuff.config.httpPort Request, Sub.map UserMsg (stuff.serverSubscriptions stuff.initServer)]

    in Worker.programWithFlags (\_ -> Cmd.none) -- TODO command line arguments??
          { init = wrapInit
          , update = wrapUpdate
          , subscriptions = wrapSubscriptions
          }

handle : Config -> (procedure -> RemoteProcedure serverModel msg) -> HttpServer.Request -> serverModel -> (serverModel, Cmd (MyMsg serverMsg))
handle { clientFile } procedures request model =
  let pathList = List.filter (not << String.isEmpty) (String.split "/" request.path)
      invalidRequest = \message -> (model, HttpServer.reply request (encodeResponse (Response Nothing message)))
  in case request.method of
    GET -> case pathList of
      [] -> case clientFile of
        Just filename -> (model, HttpServer.replyFile request filename)
        _ -> invalidRequest "Invalid request"

      _ -> invalidRequest "Invalid request"
    POST -> case pathList of
      ["procedure"] -> let (RP _ toTask) = procedures (Native.Multitier.fromJSON request.body) in
        let (newModel, task) = toTask model in
          (newModel, Task.perform (\err -> Reply request "Procedure failed" Nothing) (\value -> Reply request "" (Just value)) task)
      _ -> invalidRequest "Invalid request"
    _ -> invalidRequest "Invalid request"

updateHelp : (a -> b) -> (model, Cmd a) -> (model, Cmd b)
updateHelp func (model, cmds) =
  (model, Cmd.map func cmds)

program :
       ProgramType
    -> { config: Config
       , initServer: serverModel
       , procedures : procedure -> RemoteProcedure serverModel msg
       , updateServer : serverMsg -> serverModel -> (serverModel, Cmd serverMsg)
       , serverSubscriptions : serverModel -> Sub serverMsg
       , init : ( model, MultitierCmd procedure msg )
       , update : msg -> model -> ( model, MultitierCmd procedure msg )
       , subscriptions : model -> Sub msg
       , view : model -> Html msg
       }
    -> Program Never
program ptype { config, initServer, procedures, updateServer, serverSubscriptions, init, update, subscriptions, view } =
    programWithFlags ptype
        { config = config
        , initServer = initServer
        , procedures = procedures
        , updateServer = updateServer
        , serverSubscriptions = serverSubscriptions
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
