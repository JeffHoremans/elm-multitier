module Chat exposing (..)

import Html exposing (..)
import Html.Events as E
import Html.Attributes
import Task exposing (Task)
import Time exposing (Time, second, minute)

import Multitier exposing (MultitierCmd(..), Config, none, batch, performOnServer, map, (!!))
import Multitier.Procedure as Proc exposing (remoteProcedure, RemoteProcedure)
import Multitier.Error exposing (Error(..))
import Server.Console as Console

-- MULTITIER - CONFIG

config: Config
config = { httpPort = 8081
         , hostname = "localhost" }

-- MULTITIER - PROCEDURES

type alias ServerModel = { messages: List String }

initServer : ServerModel
initServer = ServerModel []

type Procedure = GetMessages | SendMessage String

procedures : Procedure -> RemoteProcedure ServerModel Msg
procedures proc = case proc of
  GetMessages ->           remoteProcedure SetMessages   (\serverModel -> (serverModel, Task.succeed serverModel.messages))
  SendMessage message ->   remoteProcedure SetMessages   (\serverModel -> let newMessages = message :: serverModel.messages in
                                                                                     ({ serverModel | messages = newMessages }, Task.succeed newMessages))
type ServerMsg = Log | Nothing

updateServer : ServerMsg -> ServerModel -> (ServerModel, Cmd ServerMsg)
updateServer msg serverModel = case msg of
  Log -> serverModel ! [Task.attempt (always Nothing) (Console.log serverModel.messages)]
  Nothing -> serverModel ! []

serverSubscriptions : ServerModel -> Sub ServerMsg
serverSubscriptions serverModel = Time.every minute (always Log)

-- INPUT

type alias Input = { messages: List String }

serverState: ServerModel -> Input
serverState {messages} = Input messages

-- MODEL

type alias Model = { input: String
                   , messages: List String
                   , error: String}

init : Input -> ( Model, MultitierCmd Procedure Msg)
init {messages} = (Model "" messages "",  batch [])

type Msg = OnInput String | Send |
           SetMessages (Result Error (List String)) |
           Tick | None

update : Msg -> Model -> ( Model, MultitierCmd Procedure Msg )
update msg model =
    case msg of
      OnInput text -> ({ model | input = text}, none)
      Send -> { model | input = "" } !! [performOnServer (SendMessage model.input)]
      SetMessages result -> case result of
        Ok messages -> { model | messages = messages } !! []
        _ -> ({ model | error = "error"}, none)
      Tick -> model !! [performOnServer GetMessages]

      None -> ( model, none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch [Time.every second (always Tick)]

-- VIEW

messages : List String -> Html Msg
messages list = Html.div [Html.Attributes.style [("overflow", "scroll"), ("width", "400px"), ("height", "400px")]] (List.map (\message -> Html.p [] [Html.text (message), Html.br [] []]) list)

view : Model -> Html Msg
view model =
  Html.body [] [
  Html.node "link" [Html.Attributes.rel "stylesheet", Html.Attributes.href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"] [],
  Html.div [Html.Attributes.class "container"] [
    Html.div [Html.Attributes.class "row"] [
      Html.div [Html.Attributes.class "col-xs-5"] [
        Html.h1 [] [ Html.text "Multitier Elm - Client"],
        messages model.messages,
        Html.div [] [
        Html.input [E.onInput OnInput, Html.Attributes.value model.input] [],
        Html.button [E.onClick Send] [text "Send"]]]]]]
