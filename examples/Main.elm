module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Events as E
import Html.Attributes
import Task exposing (Task)
import Time exposing (Time, second)

import Multitier exposing (MultitierCmd(..), Config, none, batch, performOnServer, map, (!!))
import Multitier.Procedure as Proc exposing (remoteProcedure, RemoteProcedure)
import Multitier.Error exposing (Error(..))
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

type Procedure = DupVal String | Log String | GetMessages | SendMessage String | CounterProc Counter.Procedure

proceduresMap : Procedure -> RemoteProcedure ServerModel Msg
proceduresMap proc = case proc of
  DupVal val ->           remoteProcedure HandleError HandleSuccess (\model -> (model, Task.succeed (val ++ val)))
  Log val ->              remoteProcedure HandleError (always None) (\model -> (model, Console.log val))
  GetMessages ->          remoteProcedure HandleError SetMessages   (\model -> (model, Task.succeed model.messages))
  SendMessage message ->  remoteProcedure HandleError SetMessages   (\model -> let newMessages = message :: model.messages in
                                                                               ({ model | messages = newMessages }, Task.succeed newMessages))
  CounterProc proc ->     Proc.map CounterMsg (\counter model -> { model | counter = counter}) (\model -> model.counter) (Counter.proceduresMap proc)

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
           Tick | CounterMsg Counter.Msg | None

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
