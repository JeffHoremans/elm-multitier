module ClientStarter exposing (..)

import Multitier exposing (ProgramType(..))
import Chat exposing (..)

main : Program Never Model Msg
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
