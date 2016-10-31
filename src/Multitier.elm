module Multitier
  exposing ( MultitierCmd
           , map
           , batch
           , none
           , (!!)

           , performOnClient
           , performOnServer0
           , performOnServer1

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
import Multitier.Callstack as Callstack
import Multitier.Procedure exposing (..)

type MultitierCmd msg = ServerCmd (RemoteProcedureCall msg) | ClientCmd (Cmd msg) | Batch (List (MultitierCmd msg))

performOnServer0 : Procedure0 msg -> MultitierCmd msg
performOnServer0 (Proc0 identifier handlers m0) = ServerCmd (RPC identifier handlers A0)

performOnServer1 : Procedure1 a msg -> a -> MultitierCmd msg
performOnServer1 (Proc1 identifier (_,encodeA) handlers m1) a = ServerCmd (RPC identifier handlers (A1 (encodeA a)))

performOnServer2 : Procedure2 a b msg -> a -> b -> MultitierCmd msg
performOnServer2 (Proc2 identifier (_,encodeA) (_,encodeB) handlers m2) a b = ServerCmd (RPC identifier handlers (A2 (encodeA a) (encodeB b)))

performOnClient : Cmd msg -> MultitierCmd msg
performOnClient cmd = ClientCmd cmd

map : (a -> msg) -> MultitierCmd a -> MultitierCmd msg
map f mtcmd = case mtcmd of
  ServerCmd (RPC identifier handlers pc) -> ServerCmd (RPC identifier (mapHandlers f handlers) pc)
  ClientCmd cmd -> ClientCmd (Cmd.map f cmd)
  Batch cmds    -> Batch (List.map (map f) cmds)

batch : List (MultitierCmd msg) -> MultitierCmd msg
batch = Batch

none : MultitierCmd msg
none = batch []

(!!) : model -> List (MultitierCmd msg) -> (model, MultitierCmd msg)
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

type alias PostData = { identifier: Identifier,
                        arguments: Arguments }

decodePostData : Decoder PostData
decodePostData = object2 PostData
  ("identifier" := Decode.string)
  ("arguments"  := decodeArguments)

encodePostData : PostData -> Value
encodePostData data = Encode.object
  [ ("identifier", Encode.string data.identifier)
  , ("arguments", encodeArguments data.arguments) ]

encodeArguments : Arguments -> Value
encodeArguments arguments = Encode.list (case arguments of
  A0 -> []
  A1 a -> [a]
  A2 a b -> [a,b]
  A3 a b c -> [a,b,c]
  A4 a b c d -> [a,b,c,d]
  A5 a b c d e -> [a,b,c,d,e]
  A6 a b c d e f -> [a,b,c,d,e,f]
  A7 a b c d e f g -> [a,b,c,d,e,f,g])

decodeArguments : Decoder Arguments
decodeArguments = Decode.customDecoder (Decode.list Decode.value)
  (\list -> case list of
    [] -> Ok A0
    [a] -> Ok (A1 a)
    [a,b] -> Ok (A2 a b)
    [a,b,c] -> Ok (A3 a b c)
    [a,b,c,d] -> Ok (A4 a b c d)
    [a,b,c,d,e] -> Ok (A5 a b c d e)
    [a,b,c,d,e,f] -> Ok (A6 a b c d e f)
    [a,b,c,d,e,f,g] -> Ok (A7 a b c d e f g)
    _ -> Err "Invalid number of arguments")

type ProgramType = OnServer | OnClient

type alias Config = { httpPort: Int
                    , hostname: String
                    , clientFile: Maybe String }

-- TODO: send arguments
unbatch : ProgramType -> Config -> MultitierCmd msg -> Cmd msg
unbatch ptype config mtcmd =
  case mtcmd of
    ServerCmd (RPC identifier (onError,onSucceed) arguments) -> case ptype of
      OnServer -> Cmd.none
      OnClient -> Task.perform onError onSucceed
        ((
            -- Http.get decodeResponse ("http://" ++ config.hostname ++ ":" ++ (toString config.httpPort) ++ "/call/" ++ identifier)
            Exts.Http.postJson
              decodeResponse
              ("http://" ++ config.hostname ++ ":" ++ (toString config.httpPort) ++ "/call/" ++ identifier)
              (Http.string (Encode.encode 4 (encodePostData (PostData identifier arguments))))
          |> Task.mapError (\err -> NetworkError err))
            `andThen` \response -> case response.data of
              Just data -> Task.succeed data
              _ -> Task.fail (ServerError response.message))

    ClientCmd cmd -> case ptype of
      OnServer -> Cmd.none
      OnClient -> cmd

    Batch cmds    -> Cmd.batch (List.map (unbatch ptype config) cmds)




type MyMsg msg = UserMsg msg | Request HttpServer.Request | Reply HttpServer.Request String (Maybe Value)

registerProcedures : List RemoteProcedure -> Cmd msg
registerProcedures procedures = Cmd.batch (List.map (\procedure -> Callstack.addProcedure procedure) procedures)

unwrapInit :
     ProgramType
  -> Config
  -> List RemoteProcedure
  -> ( model, MultitierCmd msg)
  -> ( model, Cmd msg )
unwrapInit ptype config procedures ( model, cmds) = (model, Cmd.batch [ registerProcedures procedures, unbatch ptype config cmds])

unwrapInitWithFlags :
     ProgramType
  -> Config
  -> List RemoteProcedure
  -> ( flags -> ( model, MultitierCmd msg ))
  -> ( flags -> ( model, Cmd msg ))
unwrapInitWithFlags ptype config procedures init =
  \flags ->
    let ( model, cmds) = init flags
    in  ( model, Cmd.batch [ registerProcedures procedures, unbatch ptype config cmds])

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
       , procedures: List RemoteProcedure
       , init : flags -> ( model, MultitierCmd msg )
       , update : msg -> model -> ( model, MultitierCmd msg )
       , subscriptions : model -> Sub msg
       , view : model -> Html msg
       }
    -> Program flags
programWithFlags ptype stuff = case ptype of
    OnClient -> App.programWithFlags
          { init = unwrapInitWithFlags OnClient stuff.config stuff.procedures stuff.init
          , update = unwrapUpdate OnClient stuff.config stuff.update
          , subscriptions = stuff.subscriptions
          , view = stuff.view
          }
    OnServer -> let update = unwrapUpdate OnServer stuff.config stuff.update
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
                     wrapInit flags = updateHelp UserMsg ((unwrapInitWithFlags OnServer stuff.config stuff.procedures stuff.init) flags)
      in App.programWithFlags
            { init = wrapInit
            , update = wrapUpdate
            , subscriptions = wrapSubscriptions
            , view = wrapView
            }

handle : Config -> HttpServer.Request -> model -> (model, Cmd msg)
handle { clientFile } request model =
  let pathList = List.filter (not << String.isEmpty) (String.split "/" request.path)
      invalidRequest = \message -> (model, HttpServer.reply request (encodeResponse (Response Nothing message)))
  in case request.method of
    GET -> case pathList of
      [] -> case clientFile of
        Just filename -> (model, HttpServer.replyFile request filename)
        _ -> invalidRequest "Invalid request"

      [ "call", identifier ] -> (model, Callstack.callProcedure identifier A0 request)

      _ -> invalidRequest "Invalid request"
    POST -> let res = Decode.decodeString decodePostData request.body
      in case res of
        Ok result -> (model, Callstack.callProcedure result.identifier result.arguments request)
        Err err -> invalidRequest "Malformed body"
    _ -> invalidRequest "Invalid request"

updateHelp : (a -> b) -> (model, Cmd a) -> (model, Cmd b)
updateHelp func (model, cmds) =
  (model, Cmd.map func cmds)

program :
       ProgramType
    -> { config: Config
       , procedures: List RemoteProcedure
       , init : ( model, MultitierCmd msg )
       , update : msg -> model -> ( model, MultitierCmd msg )
       , subscriptions : model -> Sub msg
       , view : model -> Html msg
       }
    -> Program Never
program ptype { config, procedures, init, update, subscriptions, view } =
    programWithFlags ptype
        { config = config
        , procedures = procedures
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
