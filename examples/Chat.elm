module Chat exposing (..)

import Html exposing (..)
import Html.Events as E
import Html.Attributes
import Task exposing (Task)
import Time exposing (Time, second)

import Multitier exposing (MultitierCmd(..), Config, none, batch, performOnServer, map, (!!))
import Multitier.Procedure as Proc exposing (remoteProcedure, RemoteProcedure)
import Multitier.Error exposing (Error(..))

-- MULTITIER - CONFIG

config: Config
config = { httpPort = 8081
         , hostname = "localhost" }

-- MULTITIER - PROCEDURES

type alias ServerModel = { messages: List (String, String) }

initServer : ServerModel
initServer = ServerModel []

type Procedure = GetMessages | SendMessage String String

procedures : Procedure -> RemoteProcedure ServerModel Msg
procedures proc = case proc of
  GetMessages ->                    remoteProcedure HandleError SetMessages   (\serverModel -> (serverModel, Task.succeed serverModel.messages))
  SendMessage username message ->   remoteProcedure HandleError SetMessages   (\serverModel -> let newMessages = (username, message) :: serverModel.messages in
                                                                                     ({ serverModel | messages = newMessages }, Task.succeed newMessages))
type ServerMsg = Nothing

updateServer : ServerMsg -> ServerModel -> (ServerModel, Cmd ServerMsg)
updateServer msg serverModel = case msg of
  Nothing -> serverModel ! []

serverSubscriptions : ServerModel -> Sub ServerMsg
serverSubscriptions serverModel = Sub.none

-- INPUT

type alias Input = { messages: List (String, String) }

serverState: ServerModel -> Input
serverState {messages} = Input messages

-- MODEL

type alias Model = { input: String
                   , username: String
                   , messages: List (String, String)
                   , error: String}

init : Input -> ( Model, MultitierCmd Procedure Msg)
init {messages} = (Model "" "" messages "",  batch [])

type Msg = OnInput String | Send |
           SetMessages (List (String, String)) |
           HandleError Error | HandleSuccess String |
           Tick | None

update : Msg -> Model -> ( Model, MultitierCmd Procedure Msg )
update msg model =
    case msg of
      OnInput text -> ({ model | input = text}, none)
      Send -> { model | input = "" } !! [performOnServer (SendMessage model.username model.input)]
      HandleError err -> ({ model | error = "error"}, none)
      HandleSuccess val -> (model, none)
      SetMessages messages -> { model | messages = messages } !! []
      Tick -> model !! [performOnServer GetMessages]

      None -> ( model, none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch [Time.every 5000 (always Tick)]

-- VIEW

messages : List (String, String) -> Html Msg
messages list = Html.div [Html.Attributes.style [("overflow", "scroll"), ("width", "400px"), ("height", "400px")]] (List.map (\(username,message) -> Html.p [] [Html.text (message), Html.br [] []]) list)

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
