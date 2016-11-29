module ClientStarter exposing (..)

import Multitier exposing (ProgramType(..))
import Chat

main : Program Never
main =
    Multitier.program OnClient
        { config = Chat.config
        , procedures = Chat.procedures
        , init = Chat.init
        , view = Chat.view
        , update = Chat.update
        , subscriptions = Chat.subscriptions
        , initServer = Chat.initServer
        , serverState = Chat.serverState
        , updateServer = Chat.updateServer
        , serverSubscriptions = Chat.serverSubscriptions
        }
