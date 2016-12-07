module ServerStarter exposing (..)

import Multitier exposing (ProgramType(..), MultitierMsg)
import Chat exposing (..)

main : Program Never ServerModel (MultitierMsg ServerMsg)
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
