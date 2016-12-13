module ClientStarter exposing (..)

import Multitier
import Main exposing (..)

main : Program String Model (Multitier.ClientMsg Msg)
main =
    Multitier.clientProgram
        { config = config
        , procedures = procedures
        , init = init
        , view = view
        , update = update
        , stateUpdate = stateUpdate
        , subscriptions = subscriptions
        , initServer = initServer
        , serverState = serverState
        , updateServer = updateServer
        , serverSubscriptions = serverSubscriptions
        }
