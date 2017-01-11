module ServerStarter exposing (..)

import Multitier
import Main exposing (..)

main : Program Never ServerModel (Multitier.ServerMsg ServerMsg)
main =
    Multitier.serverProgram
        { config = config
        , initServer = initServer
        , serverState = serverState
        , procedures = procedures
        , serverSubscriptions = serverSubscriptions }
