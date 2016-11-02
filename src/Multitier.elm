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

import HttpServer
import HttpServer.Utils exposing (Method(..))
import Multitier.Error exposing (Error(..))
import Multitier.Procedure exposing (..)
import Multitier.Type exposing (Type)

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
  ("data"     := Decode.maybe Decode.value)
  ("message"  := Decode.string)

encodeResponse : Response -> Value
encodeResponse response = Encode.object
  [ ("data", (case response.data of
                Just data -> data
                _ -> Encode.null))
  , ("message", Encode.string response.message) ]



type ProgramType = OnServer | OnClient

type alias Config = { httpPort: Int
                    , hostname: String
                    , clientFile: Maybe String }

unbatch : ProgramType -> Config -> Type procedure -> (procedure -> RemoteProcedure msg) -> MultitierCmd procedure msg -> Cmd msg
unbatch ptype config (decodeProcedure,encodeProcedure) procedures mtcmd =
  case mtcmd of
    ServerCmd procedure ->
      let (RP (onError,onSucceed) task) = procedures procedure in
      case ptype of
        OnServer -> Cmd.none
        OnClient -> Task.perform onError onSucceed
          ((
              Exts.Http.postJson
                decodeResponse
                ("http://" ++ config.hostname ++ ":" ++ (toString config.httpPort) ++ "/procedure/")
                (Http.string (Encode.encode 4 (encodeProcedure procedure)))

            |> Task.mapError (\err -> NetworkError err))
              `andThen` \response -> case response.data of
                Just data -> Task.succeed data
                _ -> Task.fail (ServerError response.message))

    ClientCmd cmd -> case ptype of
      OnServer -> Cmd.none
      OnClient -> cmd

    Batch cmds    -> Cmd.batch (List.map (unbatch ptype config (decodeProcedure,encodeProcedure) procedures) cmds)




type MyMsg msg = UserMsg msg | Request HttpServer.Request | Reply HttpServer.Request String (Maybe Value)

unwrapInit :
     ProgramType
  -> Config
  -> Type procedure
  -> (procedure -> RemoteProcedure msg)
  -> ( model, MultitierCmd procedure msg)
  -> ( model, Cmd msg )
unwrapInit ptype config coder procedures ( model, cmds) = (model, unbatch ptype config coder procedures cmds)

unwrapInitWithFlags :
     ProgramType
  -> Config
  -> Type procedure
  -> (procedure -> RemoteProcedure msg)
  -> ( flags -> ( model, MultitierCmd procedure msg ))
  -> ( flags -> ( model, Cmd msg ))
unwrapInitWithFlags ptype config coder procedures init =
  \flags ->
    let ( model, cmds) = init flags
    in  ( model, unbatch ptype config coder procedures cmds)

unwrapUpdate :
     ProgramType
  -> Config
  -> Type procedure
  -> (procedure -> RemoteProcedure msg)
  -> (msg -> model -> ( model, MultitierCmd procedure msg ))
  -> (msg -> model -> ( model, Cmd msg ))
unwrapUpdate ptype config coder procedures update =
  \msg model ->
    let (newModel, cmds) = update msg model
    in  (newModel, unbatch ptype config coder procedures cmds)

programWithFlags :
       ProgramType
    -> { config: Config
       , coder: Type procedure
       , procedures: procedure -> RemoteProcedure msg
       , init : flags -> ( model, MultitierCmd procedure msg )
       , update : msg -> model -> ( model, MultitierCmd procedure msg )
       , subscriptions : model -> Sub msg
       , view : model -> Html msg
       }
    -> Program flags
programWithFlags ptype stuff = case ptype of
  OnClient -> App.programWithFlags
        { init = unwrapInitWithFlags OnClient stuff.config stuff.coder stuff.procedures stuff.init
        , update = unwrapUpdate OnClient stuff.config stuff.coder stuff.procedures stuff.update
        , subscriptions = stuff.subscriptions
        , view = stuff.view
        }
  OnServer -> let update = unwrapUpdate OnServer stuff.config stuff.coder stuff.procedures stuff.update
            in let wrapUpdate msg model =
                    case msg of
                      UserMsg userMsg -> updateHelp UserMsg <| update userMsg model
                      Request request -> handle stuff.config stuff.coder stuff.procedures request model
                      Reply request message data -> updateHelp UserMsg <| (model, HttpServer.reply request (encodeResponse (Response data message)))
                   wrapSubscriptions model = Sub.batch
                    [ HttpServer.listen stuff.config.httpPort Request, Sub.map UserMsg (stuff.subscriptions model)]
                   wrapView model =
                     App.map UserMsg (stuff.view model)
                   wrapInit flags = updateHelp UserMsg ((unwrapInitWithFlags OnServer stuff.config stuff.coder stuff.procedures stuff.init) flags)
    in App.programWithFlags
          { init = wrapInit
          , update = wrapUpdate
          , subscriptions = wrapSubscriptions
          , view = wrapView
          }


handle : Config -> Type procedure -> (procedure -> RemoteProcedure msg) -> HttpServer.Request -> model -> (model, Cmd (MyMsg msg))
handle { clientFile } (decodeProcedure,_) procedures request model =
  let pathList = List.filter (not << String.isEmpty) (String.split "/" request.path)
      invalidRequest = \message -> (model, HttpServer.reply request (encodeResponse (Response Nothing message)))
  in case request.method of
    GET -> case pathList of
      [] -> case clientFile of
        Just filename -> (model, HttpServer.replyFile request filename)
        _ -> invalidRequest "Invalid request"

      _ -> invalidRequest "Invalid request"
    POST -> case pathList of
      ["procedure"] -> let res = Decode.decodeString decodeProcedure request.body
        in case res of
          Ok result -> let (RP _ task) = procedures result
            in (model, Task.perform (\err -> Reply request "Procedure failed" Nothing) (\value -> Reply request "" (Just value)) task)
          Err err -> invalidRequest ("Malformed body: " ++ err)
      _ -> invalidRequest "Invalid request"
    _ -> invalidRequest "Invalid request"

updateHelp : (a -> b) -> (model, Cmd a) -> (model, Cmd b)
updateHelp func (model, cmds) =
  (model, Cmd.map func cmds)

program :
       ProgramType
    -> { config: Config
       , coder: Type procedure
       , procedures: procedure -> RemoteProcedure msg
       , init : ( model, MultitierCmd procedure msg )
       , update : msg -> model -> ( model, MultitierCmd procedure msg )
       , subscriptions : model -> Sub msg
       , view : model -> Html msg
       }
    -> Program Never
program ptype { config, coder, procedures, init, update, subscriptions, view } =
    programWithFlags ptype
        { config = config
        , coder = coder
        , procedures = procedures
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
