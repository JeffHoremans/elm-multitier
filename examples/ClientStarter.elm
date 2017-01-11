module ClientStarter exposing (..)

import Multitier
import Main exposing (..)

main : Program String Model (Multitier.ClientMsg Msg)
main =
    Multitier.clientProgram
        { config = config
        , init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , serverState = serverState
        , procedures = procedures
        }
