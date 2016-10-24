module ClientStarter exposing (..)

import Multitier exposing (ProgramType(..))
import Main

main : Program Never
main =
    Multitier.program Client
        { config = Main.config
        , init = Main.init
        , view = Main.view
        , update = Main.update
        , subscriptions = Main.subscriptions
        }
