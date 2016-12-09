module Server.File
    exposing
        ( read
        , write
        )

import Native.Server.File
import Task exposing (Task)

read : String -> Task String String
read path = Native.Server.File.read path

write : String -> String -> Task String ()
write path data = Native.Server.File.write path data
