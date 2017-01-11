module Main exposing (..)

import Html exposing (Html)
import Html.Events as E
import Html.Attributes
import Task exposing (Task)
import Time exposing (Time, second)

import Multitier exposing (MultitierCmd(..), Config, none, batch, performOnServer, map, (!!))
import Multitier.Procedure as Procedure exposing (procedure, Procedure)
import Multitier.Error exposing (Error(..))
import Multitier.Server.Console as Console

import Counter

-- MULTITIER - CONFIG

config: Config
config = { httpPort = 8081, hostname = "localhost" }

-- MULTITIER - PROCEDURES

type alias ServerModel = { messages: List String
                         , counter: Counter.ServerModel }

initServer : ServerModel
initServer = ServerModel [] Counter.initServer

type Proc = Log String | SendMessage String | CounterProc Counter.Proc

procedures : Proc -> Procedure ServerModel Msg
procedures proc = case proc of
  Log val ->              procedure Handle (\serverModel -> (serverModel, Console.log val))
  SendMessage message ->  procedure Handle (\serverModel -> let newMessages = message :: serverModel.messages in
                                           ({ serverModel | messages = newMessages }, Task.succeed ()))
  CounterProc proc ->     Procedure.map CounterMsg (\counter serverModel -> { serverModel | counter = counter})
                                                   (\serverModel -> serverModel.counter) (Counter.proceduresMap proc)

type ServerMsg = ServerTick | CounterServerMsg Counter.ServerMsg | Nothing

updateServer : ServerMsg -> ServerModel -> (ServerModel, Cmd ServerMsg)
updateServer msg serverModel = case msg of
  ServerTick -> serverModel ! [Task.attempt (always Nothing) (Console.log (toString serverModel.messages))]
  CounterServerMsg msg -> let (counter, cmds) = Counter.updateServer msg serverModel.counter in { serverModel | counter = counter} ! [Cmd.map CounterServerMsg cmds]
  Nothing -> serverModel ! []

serverSubscriptions : ServerModel -> Sub ServerMsg
serverSubscriptions serverModel = Sub.batch [Time.every 10000 (always ServerTick), Sub.map CounterServerMsg (Counter.serverSubscriptions serverModel.counter)]

-- INPUT

type alias ServerState = { messages: List String }

serverState: ServerModel -> ServerState
serverState {messages} = ServerState messages

-- MODEL

type alias Model = { input: String
                   , messages: List String
                   , error: String
                   , counter: Counter.Model }

init : ServerState -> ( Model, MultitierCmd Proc Msg)
init {messages} = let (counter, cmds) = Counter.init
       in (Model "" messages "" counter,  batch [ map CounterProc CounterMsg cmds ])

type Msg = OnInput String | Send |
           Handle (Result Error ()) |
           CounterMsg Counter.Msg | None

update : Msg -> Model -> ( Model, MultitierCmd Proc Msg )
update msg model =
    case msg of
      OnInput text -> ({ model | input = text}, none)
      Send -> { model | input = "" } !! [performOnServer (SendMessage model.input)]
      Handle result -> case result of
        Ok _ -> model !! []
        _ -> { model | error = "error" } !! []
      CounterMsg subMsg -> let (counter, cmds) = Counter.update subMsg model.counter in { model | counter = counter } !! [ map CounterProc CounterMsg cmds ]
      None -> ( model, none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch [Sub.map CounterMsg (Counter.subscriptions model.counter)]

-- VIEW

view : Model -> Html Msg
view model =
  Html.body [] [
  Html.node "link" [Html.Attributes.rel "stylesheet", Html.Attributes.href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"] [],
  Html.div [] [
    Html.h1 [] [ Html.text "Multitier Elm - Client"],
    Html.div [] [
    Html.input [E.onInput OnInput, Html.Attributes.value model.input] [],
    Html.button [E.onClick Send] [Html.text "Send"]
    ],
    Html.div [] [
      Html.text (toString model.messages),
      Html.br [] [],
      Html.text model.error],
    Html.div [] [Html.map CounterMsg (Counter.view model.counter)]]]
