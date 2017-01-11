module Multitier.Server.File
    exposing
        ( read
        , write
        , exists
        )

import Native.Multitier.Server.File
import Task exposing (Task)

read : String -> Task String String
read path = Native.Multitier.Server.File.read path

write : String -> String -> Task String ()
write path data = Native.Multitier.Server.File.write path data

exists : String -> Bool
exists = Native.Multitier.Server.File.exists
