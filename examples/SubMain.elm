module SubMain exposing (..)

import Html exposing (..)

import Multitier exposing (MultitierCmd(..), Procedures, Procedure, Config, none, batch, (!!))

type alias Model = { test: String }

procedures : Procedures Msg
procedures = Multitier.defineProcedures []

init : ( Model, MultitierCmd Msg)
init = Model "" !! []

type Msg = None

update : Msg -> Model -> ( Model, MultitierCmd Msg )
update msg model =
    case msg of
      None -> ( model, none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  Html.div [] [Html.text model.test]
