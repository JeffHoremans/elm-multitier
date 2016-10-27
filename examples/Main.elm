module Main exposing (..)

import Html exposing (..)
import Html.Events as E
import Html.Attributes
import Task exposing (Task)

import Multitier exposing (MultitierCmd(..), Config, none, batch, map, (!!))
import Multitier.Procedure exposing (Procedure0, Procedure1, RemoteProcedure, procedure0, procedure1, rp0, rp1)
import Multitier.Error exposing (Error(..))
import Multitier.Type exposing (string)

import Server.Console as Console

type alias Model = { input: String, messages: List String, error: String }


config: Config
config = { httpPort = 8081
         , hostname = "localhost"
         , clientFile = Just "examples/index.html" }

procedures : List RemoteProcedure
procedures = [ rp0 test, rp1 test2]

init : ( Model, MultitierCmd Msg)
init = Model "" [] "" !! [
                          dupValServer0
                          --logOnServer "Message from browser"
                         ]

type Msg = Input String | Send | HandleError Error | HandleSuccess String |
           None

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

test : Procedure0 Msg
test = procedure0 "test" HandleError HandleSuccess string (dupVal "test")

test2 : Procedure1 String Msg
test2 = procedure1 "test2" HandleError HandleSuccess string string dupVal

dupValServer0 : MultitierCmd Msg
dupValServer0 = Multitier.performOnServer0 test

dupValServer1 : String -> MultitierCmd Msg
dupValServer1 val = Multitier.performOnServer1 test2 val

dupValClient : String -> MultitierCmd Msg
dupValClient val = Multitier.performOnClient (Task.perform HandleError HandleSuccess (dupVal val))

log : Procedure1 String Msg
log = Console.log "log" HandleError (\_ -> HandleSuccess "success")

logServer : String -> MultitierCmd Msg
logServer val = Multitier.performOnServer1 log val

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
