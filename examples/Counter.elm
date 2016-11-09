module Counter
  exposing ( .. )

import Html exposing (..)
import Html.Events as E
import Json.Decode as Decode exposing (Decoder,(:=), andThen)
import Json.Encode as Encode exposing (Value)
import Task exposing (Task)

import Multitier exposing (MultitierCmd(..), Config, none, batch, (!!), performOnServer)
import Multitier.Procedure exposing (RemoteProcedure, remoteProcedure)
import Multitier.Type exposing (Type, int)
import Multitier.Error exposing (Error)

type alias Model = { value: Int, error: String }

-- MULTITIER - PROCEDURES

type alias ServerModel = { test: String }

initServer : ServerModel
initServer = ServerModel ""

type Procedure = Add Int Int

updateServer : Procedure -> ServerModel -> (ServerModel, RemoteProcedure Msg)
updateServer proc model = case proc of
  Add a b -> (model, remoteProcedure HandleError HandleSuccess int (add a b))

decodeProcedure : Decoder Procedure
decodeProcedure =
  ("type" := Decode.string) `andThen` decodeArguments

decodeArguments : String -> Decoder Procedure
decodeArguments procType =
  case procType of
    "add" ->    Decode.object2 Add ("a" := Decode.int) ("b" := Decode.int)
    _ ->        Decode.fail (procType ++ " is not a recognized type")

encodeProcedure : Procedure -> Value
encodeProcedure proc = case proc of
  Add a b -> Encode.object [ ("type", Encode.string "add"), ("a", Encode.int a), ("b", Encode.int b)]

add : Int -> Int -> Task x Int
add a b = Task.succeed (a + b)

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
