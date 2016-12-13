module ServerStarter exposing (..)

import Multitier
import Main exposing (..)

main : Program Never ServerModel (Multitier.ServerMsg ServerMsg)
main =
    Multitier.serverProgram
        { config = config
        , procedures = procedures
        , init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , initServer = initServer
        , serverState = serverState
        , updateServer = updateServer
        , serverSubscriptions = serverSubscriptions }
