module ClientStarter exposing (..)

import Multitier exposing (ProgramType(..))
import Main

main : Program Never
main =
    Multitier.program OnClient
        { config = Main.config
        , codec = Main.codec
        , procedures = Main.procedures
        , init = Main.init
        , view = Main.view
        , update = Main.update
        , subscriptions = Main.subscriptions
        }
