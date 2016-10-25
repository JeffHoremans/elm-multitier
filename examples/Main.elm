module Main exposing (..)

import Html exposing (..)
import Html.Events as E
import Html.Attributes
import Task exposing (Task)
import Json.Decode as Decode
import Json.Encode as Encode

import Multitier exposing (MultitierCmd(..), Config, none, batch, (!!))
import Multitier.Error exposing (Error(..))
import Server.Console as Console

type alias Model = { input: String, messages: List String, error: String }


config: Config
config = { httpPort = 8081, hostname = "localhost", clientFile = Just "examples/index.html" }

init : ( Model, MultitierCmd Msg)
init = Model "" [] "" !! [
                            dupValServer "test"
                            --logOnServer "Message from browser"
                         ]

type Msg = Input String | Send | HandleError Error | HandleSuccess String | None

update : Msg -> Model -> ( Model, MultitierCmd Msg )
update msg model =
    case msg of
      Input text -> ({ model | input = text}, none)
      Send -> ({ model | input = "" }, dupValClient model.input)
      HandleError err -> ({ model | error = "error"}, none)
      HandleSuccess val -> { model | messages = val :: model.messages } !! [none]
      None -> ( model, none )

dupVal : String -> Task x String
dupVal val = Task.succeed(val ++ val)
--
dupValServer : String -> MultitierCmd Msg
dupValServer val = Multitier.performOnServer HandleError HandleSuccess Decode.string Encode.string (dupVal val)
--
dupValClient : String -> MultitierCmd Msg
dupValClient val = Multitier.performOnClient (Task.perform HandleError HandleSuccess (dupVal val))

logOnServer : String -> MultitierCmd Msg
logOnServer val = Console.log val HandleError (always None)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

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
      Html.text ("Error: " ++ model.error)]]
