module Counter
  exposing ( .. )

import Html exposing (..)
import Html.Events as E
import Task exposing (Task)

import Multitier exposing (MultitierCmd(..), Config, none, batch, (!!), performOnServer)
import Multitier.Procedure exposing (Procedure, procedure)
import Multitier.Error exposing (Error)

type alias Model = { value: Int, error: String }

-- MULTITIER - PROCEDURES

type alias ServerModel = { test: String }

initServer : ServerModel
initServer = ServerModel ""

type Proc = Add Int Int

proceduresMap : Proc -> Procedure ServerModel Msg
proceduresMap proc = case proc of
  Add a b -> procedure Handle (\serverModel -> (serverModel, Task.succeed (a + b)))

type ServerMsg = Nothing

updateServer : ServerMsg -> ServerModel -> (ServerModel, Cmd ServerMsg)
updateServer _ serverModel = serverModel ! []

serverSubscriptions : ServerModel -> Sub ServerMsg
serverSubscriptions model = Sub.none

-- MODEL

init : ( Model, MultitierCmd Proc Msg)
init = Model 0 "" !! []

type Msg = Handle (Result Error Int) | Increment | None

update : Msg -> Model -> ( Model, MultitierCmd Proc Msg )
update msg model =
    case msg of
      Handle result -> case result of
        Ok val -> { model | value = val } !! []
        _ -> { model | error = "error" } !! []
      Increment -> model !! [performOnServer (Add model.value 1)]
      None -> ( model, none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  Html.button [E.onClick Increment] [Html.text (toString model.value)]
