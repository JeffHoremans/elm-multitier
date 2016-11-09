module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Events as E
import Html.Attributes
import Task exposing (Task)
import Json.Encode as Encode exposing (Value, object)
import Json.Decode as Decode exposing (Decoder, (:=), andThen)
import Time exposing (Time, second)

import Multitier exposing (MultitierCmd(..), Config, none, batch, performOnServer, map, (!!))
import Multitier.Procedure as Proc exposing (remoteProcedure, RemoteProcedure)
import Multitier.Error exposing (Error(..))
import Multitier.Type  exposing (Type, string, void, list)
import Server.Console as Console

import Counter

-- MULTITIER - CONFIG

config: Config
config = { httpPort = 8081
         , hostname = "localhost"
         , clientFile = Just "examples/index.html" }

-- MULTITIER - PROCEDURES

type alias ServerModel = { messages: List String
                         , counter: Counter.ServerModel }

initServer : ServerModel
initServer = ServerModel [] Counter.initServer

type Procedure = DupVal String | Log String |

                 GetMessages | SendMessage String |

                 CounterProc Counter.Procedure

updateServer : Procedure -> ServerModel -> (ServerModel, RemoteProcedure Msg)
updateServer proc model = case proc of
  DupVal val -> (model, remoteProcedure HandleError HandleSuccess string (dupVal val))
  Log val -> (model, remoteProcedure HandleError (always None) void (Console.log val))
  CounterProc proc -> let (counter, counterProc) = Counter.updateServer proc model.counter
    in ({ model | counter = counter }, Proc.map CounterMsg counterProc)
  GetMessages -> (model, remoteProcedure HandleError SetMessages (list string) (Task.succeed model.messages))
  SendMessage message -> let newMessages = message :: model.messages
    in ({ model | messages = newMessages }, remoteProcedure HandleError SetMessages (list string) (Task.succeed newMessages))

codec : Type Procedure
codec = (decodeProcedure,encodeProcedure)

decodeProcedure : Decoder Procedure
decodeProcedure =
  ("type" := Decode.string) `andThen` (\procType ->
    case procType of
      "dupVal" ->       Decode.object1 DupVal ("val" := Decode.string)
      "log" ->          Decode.object1 Log ("val" := Decode.string)
      "getMessages" ->  Decode.succeed GetMessages
      "sendMessage" ->  Decode.object1 SendMessage ("message" := Decode.string)
      "counter" ->      Decode.object1 CounterProc ("proc" := Counter.decodeProcedure)
      _ ->              Decode.fail (procType ++ " is not a recognized type"))

encodeProcedure : Procedure -> Value
encodeProcedure proc = case proc of
  DupVal val ->           Encode.object [ ("type", Encode.string "dupVal"), ("val",  Encode.string val) ]
  Log val ->              Encode.object [ ("type", Encode.string "log"), ("val",  Encode.string val) ]
  GetMessages ->          Encode.object [ ("type", Encode.string "getMessages") ]
  SendMessage message ->  Encode.object [ ("type", Encode.string "sendMessage"), ("message",  Encode.string message) ]
  CounterProc proc ->     Encode.object [ ("type", Encode.string "counter"), ("proc", Counter.encodeProcedure proc) ]

dupVal : String -> Task x String
dupVal val = Task.succeed (val ++ val)


-- MODEL

type alias Model = { input: String
                   , messages: List String
                   , error: String
                   , counter: Counter.Model }

init : ( Model, MultitierCmd Procedure Msg)
init = let (counter, cmds) = Counter.init
       in (Model "" [] "" counter,  batch [ map CounterProc CounterMsg cmds ])

type Msg = Input String | Send |
           SetMessages (List String) |
           HandleError Error | HandleSuccess String |
           Tick |
           CounterMsg Counter.Msg |

           None

update : Msg -> Model -> ( Model, MultitierCmd Procedure Msg )
update msg model =
    case msg of
      Input text -> ({ model | input = text}, none)
      Send -> { model | input = "" } !! [performOnServer (SendMessage model.input)]
      HandleError err -> ({ model | error = "error"}, none)
      HandleSuccess val -> { model | messages = ("Logged succesfully: " ++ val) :: model.messages } !! []
      SetMessages messages -> { model | messages = messages } !! []
      Tick -> model !! [performOnServer GetMessages]

      CounterMsg subMsg -> let (counter, cmds) = Counter.update subMsg model.counter
                           in { model | counter = counter } !! [ map CounterProc CounterMsg cmds ]

      None -> ( model, none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch [Sub.map CounterMsg (Counter.subscriptions model.counter), Time.every second (always Tick)]

-- VIEW

view : Model -> Html Msg
view model =
  Html.div [] [
    Html.h1 [] [ Html.text "Multitier Elm - Client"],
    Html.div [] [
    Html.input [E.onInput Input, Html.Attributes.value model.input] [],
    Html.button [E.onClick Send] [text "Send"]
    ],
    Html.div [] [
      Html.text (toString model.messages),
      Html.br [] [],
      Html.text model.error],
    Html.div [] [App.map CounterMsg (Counter.view model.counter)]]
