module Counter
  exposing ( .. )

import Html exposing (..)
import Html.Events as E
import Task exposing (Task)

import Multitier exposing (MultitierCmd(..), Config, none, batch, (!!), performOnServer)
import Multitier.Procedure exposing (RemoteProcedure, remoteProcedure)
import Multitier.Error exposing (Error)

type alias Model = { value: Int, error: String }

-- MULTITIER - PROCEDURES

type alias ServerModel = { test: String }

initServer : ServerModel
initServer = ServerModel ""

type Procedure = Add Int Int

proceduresMap : Procedure -> RemoteProcedure ServerModel Msg
proceduresMap proc = case proc of
  Add a b -> remoteProcedure HandleError HandleSuccess (\serverModel -> (serverModel, Task.succeed (a + b)))

type ServerMsg = Nothing

updateServer : ServerMsg -> ServerModel -> (ServerModel, Cmd ServerMsg)
updateServer _ serverModel = serverModel ! []

serverSubscriptions : ServerModel -> Sub ServerMsg
serverSubscriptions model = Sub.none

-- MODEL

init : ( Model, MultitierCmd Procedure Msg)
init = Model 0 "" !! []

type Msg = HandleError Error | HandleSuccess Int | Increment | None

update : Msg -> Model -> ( Model, MultitierCmd Procedure Msg )
update msg model =
    case msg of
      HandleError err -> ({ model | error = "error"}, none)
      HandleSuccess val -> { model | value = val } !! []
      Increment -> model !! [performOnServer (Add model.value 1)]
      None -> ( model, none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  Html.button [E.onClick Increment] [Html.text (toString model.value)]
