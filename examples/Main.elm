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
         , hostname = "localhost" }

-- MULTITIER - PROCEDURES

type alias ServerModel = { messages: List String
                         , counter: Counter.ServerModel }

initServer : ServerModel
initServer = ServerModel [] Counter.initServer

type Procedure = DupVal String | Log String | GetMessages | SendMessage String | CounterProc Counter.Procedure

procedures : Procedure -> RemoteProcedure ServerModel Msg
procedures proc = case proc of
  DupVal val ->           remoteProcedure HandleError HandleSuccess (\serverModel -> (serverModel, Task.succeed (val ++ val)))
  Log val ->              remoteProcedure HandleError (always None) (\serverModel -> (serverModel, Console.log val))
  GetMessages ->          remoteProcedure HandleError SetMessages   (\serverModel -> (serverModel, Task.succeed serverModel.messages))
  SendMessage message ->  remoteProcedure HandleError SetMessages   (\serverModel -> let newMessages = message :: serverModel.messages in
                                                                                     ({ serverModel | messages = newMessages }, Task.succeed newMessages))
  CounterProc proc ->     Proc.map CounterMsg (\counter serverModel -> { serverModel | counter = counter}) (\serverModel -> serverModel.counter) (Counter.proceduresMap proc)

type ServerMsg = ServerTick | CounterServerMsg Counter.ServerMsg | Nothing

updateServer : ServerMsg -> ServerModel -> (ServerModel, Cmd ServerMsg)
updateServer msg serverModel = case msg of
  ServerTick -> serverModel ! [Task.perform (always Nothing) (always Nothing) (Console.log (toString serverModel.messages))]
  CounterServerMsg msg -> let (counter, cmds) = Counter.updateServer msg serverModel.counter in { serverModel | counter = counter} ! [Cmd.map CounterServerMsg cmds]
  Nothing -> serverModel ! []

serverSubscriptions : ServerModel -> Sub ServerMsg
serverSubscriptions serverModel = Sub.batch [Time.every 10000 (always ServerTick), Sub.map CounterServerMsg (Counter.serverSubscriptions serverModel.counter)]

-- INPUT

type alias Input = { messages: List String }

serverState: ServerModel -> Input
serverState {messages} = Input messages

-- MODEL

type alias Model = { input: String
                   , messages: List String
                   , error: String
                   , counter: Counter.Model }

init : Input -> ( Model, MultitierCmd Procedure Msg)
init {messages} = let (counter, cmds) = Counter.init
       in (Model "" messages "" counter,  batch [ map CounterProc CounterMsg cmds ])

type Msg = OnInput String | Send |
           SetMessages (List String) |
           HandleError Error | HandleSuccess String |
           Tick | CounterMsg Counter.Msg | None

update : Msg -> Model -> ( Model, MultitierCmd Procedure Msg )
update msg model =
    case msg of
      OnInput text -> ({ model | input = text}, none)
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
  Html.body [] [
  Html.node "link" [Html.Attributes.rel "stylesheet", Html.Attributes.href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"] [],
  Html.div [] [
    Html.h1 [] [ Html.text "Multitier Elm - Client"],
    Html.div [] [
    Html.input [E.onInput OnInput, Html.Attributes.value model.input] [],
    Html.button [E.onClick Send] [text "Send"]
    ],
    Html.div [] [
      Html.text (toString model.messages),
      Html.br [] [],
      Html.text model.error],
    Html.div [] [App.map CounterMsg (Counter.view model.counter)]]]
