module Multitier
  exposing ( MultitierCmd
           , map
           , batch
           , none
           , (!!)

           , performOnClient
           , performOnServer
           , encodeVoid, decodeVoid

           , ProgramType(..)
           , Config
           , programWithFlags
           , program
           )

import Html.App as App
import Html exposing (Html)
import Task exposing (Task, andThen)
import Http
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder, object2, (:=))
import String

import HttpServer
import Multitier.Error exposing (Error(..))
import Multitier.Callstack as Callstack

type alias CallId = Int

type alias RemoteProcedure = (CallId, Procedure)

type Procedure =  P0 (Task Error Value) |
                  P1 Value (Value -> Task Error Value) |
                  P2 Value Value (Value -> Value -> Task Error Value) |
                  P3 Value Value Value (Value -> Value -> Value -> Task Error Value) |
                  P4 Value Value Value Value (Value -> Value -> Value -> Value -> Task Error Value) |
                  P5 Value Value Value Value Value (Value -> Value -> Value -> Value -> Value -> Task Error Value) |
                  P6 Value Value Value Value Value Value (Value -> Value -> Value -> Value -> Value -> Value -> Task Error Value) |
                  P7 Value Value Value Value Value Value Value (Value -> Value -> Value -> Value -> Value -> Value -> Value -> Task Error Value)

type MultitierCmd msg = ServerCmd (Error -> msg) (Value -> msg) (CallId, (Task Error Value)) | ClientCmd (Cmd msg) | Batch (List (MultitierCmd msg))

{-|-}
map : (a -> msg) -> MultitierCmd a -> MultitierCmd msg
map f mtcmd = case mtcmd of
  ServerCmd oe os (cid, task) -> ServerCmd (oe >> f) (os >> f) (cid, task)
  ClientCmd cmd -> ClientCmd (Cmd.map f cmd)
  Batch cmds    -> Batch (List.map (map f) cmds)

{-|-}
batch : List (MultitierCmd msg) -> MultitierCmd msg
batch = Batch

{-|-}
none : MultitierCmd msg
none = batch []


{-|-}
(!!) : model -> List (MultitierCmd msg) -> (model, MultitierCmd msg)
(!!) model commands =
  (model, batch commands)

encodeVoid : (() -> Value)
encodeVoid = always Encode.null

decodeVoid : Decoder ()
decodeVoid = Decode.succeed ()

performOnServer : (Error -> msg) -> (a -> msg) -> Decoder a -> (a -> Value) -> Task Error a -> MultitierCmd msg
performOnServer onError onSuccess decoder encode task =
  let mappedOnSucceed = mapOnSuccess onError onSuccess decoder
  in let mappedTask = Task.map encode task
    in ServerCmd onError mappedOnSucceed (0, mappedTask)

mapOnSuccess : (Error -> msg) -> (a -> msg) -> Decoder a -> (Value -> msg)
mapOnSuccess onError onSuccess decoder =
  \value -> let res = Decode.decodeValue decoder value
    in case res of
      Ok result -> onSuccess result
      Err err -> onError (MultitierError err)

performOnClient : Cmd msg -> MultitierCmd msg
performOnClient cmd = ClientCmd cmd


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

unbatch : ProgramType -> Config -> MultitierCmd msg -> Cmd msg
unbatch ptype config mtcmd =
  case mtcmd of
    ServerCmd onError onSucceed (callid, task) -> case ptype of
      Server -> Callstack.addCall task
      Client -> Task.perform onError onSucceed
        ((Http.get decodeResponse ("http://" ++ config.hostname ++ ":" ++ (toString config.httpPort) ++ "/call/" ++ (toString callid))
          |> Task.mapError (\err -> NetworkError err))
            `andThen` \response -> case response.data of
              Just data -> Task.succeed data
              _ -> Task.fail (ServerError response.message))

    ClientCmd cmd -> case ptype of
      Server -> Cmd.none
      Client -> cmd

    Batch cmds    -> Cmd.batch (List.map (unbatch ptype config) cmds)

type ProgramType = Server | Client
type alias Config = { httpPort: Int
                    , hostname: String
                    , clientFile: Maybe String }

type MyMsg msg = UserMsg msg | Request HttpServer.Request | Reply HttpServer.Request String (Maybe Value)

unwrapInit :
     ProgramType
  -> Config
  -> ( model, MultitierCmd msg)
  -> ( model, Cmd msg )
unwrapInit ptype config ( model, cmds) = (model, unbatch ptype config cmds)

unwrapInitWithFlags :
     ProgramType
  -> Config
  -> ( flags -> ( model, MultitierCmd msg ))
  -> ( flags -> ( model, Cmd msg ))
unwrapInitWithFlags ptype config init =
  \flags ->
    let ( model, cmds) = init flags
    in  ( model, unbatch ptype config cmds)

unwrapUpdate :
     ProgramType
  -> Config
  -> (msg -> model -> ( model, MultitierCmd msg ))
  -> (msg -> model -> ( model, Cmd msg ))
unwrapUpdate ptype config update =
  \msg model ->
    let (newModel, cmds) = update msg model
    in  (newModel, unbatch ptype config cmds)

programWithFlags :
       ProgramType
    -> { config: Config
       , init : flags -> ( model, MultitierCmd msg )
       , update : msg -> model -> ( model, MultitierCmd msg )
       , subscriptions : model -> Sub msg
       , view : model -> Html msg
       }
    -> Program flags
programWithFlags ptype stuff = case ptype of
    Client -> App.programWithFlags
          { init = unwrapInitWithFlags Client stuff.config stuff.init
          , update = unwrapUpdate Client stuff.config stuff.update
          , subscriptions = stuff.subscriptions
          , view = stuff.view
          }
    Server -> let update = unwrapUpdate Server stuff.config stuff.update
              in let wrapUpdate msg model =
                      updateHelp UserMsg
                        <| case msg of
                            UserMsg userMsg -> update userMsg model
                            Request request -> handle stuff.config request model
                            Reply request message data -> (model, HttpServer.reply request (encodeResponse (Response data message)))
                     wrapSubscriptions model = Sub.batch
                      [ Callstack.onCallComplete Reply, HttpServer.listen stuff.config.httpPort Request, Sub.map UserMsg (stuff.subscriptions model)]
                     wrapView model =
                       App.map UserMsg (stuff.view model)
                     wrapInit flags = updateHelp UserMsg ((unwrapInitWithFlags Server stuff.config stuff.init) flags)
      in App.programWithFlags
            { init = wrapInit
            , update = wrapUpdate
            , subscriptions = wrapSubscriptions
            , view = wrapView
            }

handle : Config -> HttpServer.Request -> model -> (model, Cmd msg)
handle { clientFile } request model =
  let pathList = List.filter (not << String.isEmpty) (String.split "/" ((HttpServer.getPath request)))
  in case (Debug.log "path list" pathList) of

    [] -> case clientFile of
      Just filename -> (model, HttpServer.replyFile request filename)
      _ -> (model, HttpServer.reply request (encodeResponse (Response Nothing "Invalid url")))

    [ "call", callid ] -> (model, Callstack.call 0 request)

    _ -> (model, HttpServer.reply request (encodeResponse (Response Nothing "Invalid url")))

updateHelp : (a -> b) -> (model, Cmd a) -> (model, Cmd b)
updateHelp func (model, cmds) =
  (model, Cmd.map func cmds)

program :
       ProgramType
    -> { config: Config
       , init : ( model, MultitierCmd msg )
       , update : msg -> model -> ( model, MultitierCmd msg )
       , subscriptions : model -> Sub msg
       , view : model -> Html msg
       }
    -> Program Never
program ptype { config, init, update, subscriptions, view } =
    programWithFlags ptype
        { config = config
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
