module ClientStarter exposing (..)

import Multitier exposing (ProgramType(..))
import Main exposing (..)

main : Program String Model Msg
main =
    Multitier.clientProgram
        { config = config
        , procedures = procedures
        , init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , initServer = initServer
        , serverState = serverState
        , updateServer = updateServer
        , serverSubscriptions = serverSubscriptions
        }
