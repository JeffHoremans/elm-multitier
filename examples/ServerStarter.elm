module ServerStarter exposing (..)

import Multitier exposing (ProgramType(..))
import Main

main : Program Never
main =
    Multitier.program OnServer
        { config = Main.config
        , initServer = Main.initServer
        , procedures = Main.procedures
        , updateServer = Main.updateServer
        , serverSubscriptions = Main.serverSubscriptions
        , init = Main.init
        , view = Main.view
        , update = Main.update
        , subscriptions = Main.subscriptions
        }
