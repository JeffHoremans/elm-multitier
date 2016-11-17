module ServerStarter exposing (..)

import Multitier exposing (ProgramType(..))
import Main

main : Program Never
main =
    Multitier.program OnServer
        { config = Main.config
        , initServer = Main.initServer
        , proceduresMap = Main.proceduresMap
        , init = Main.init
        , view = Main.view
        , update = Main.update
        , subscriptions = Main.subscriptions
        }
