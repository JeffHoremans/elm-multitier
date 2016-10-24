module HttpServer.LowLevel exposing
  ( listen
  , Settings
  , reply
  , getPath
  , close
  , Server
  , Request
  )

import Native.HttpServer
import Task exposing (Task)
import Json.Encode as Encode exposing (Value)

type Server = Server

type Request = Request

{-| Attempt to listen to a particular port.
-}
listen : Int -> Settings -> Task x Server
listen portNumber settings =
  let
    test = Debug.log "TEST" portNumber
  in
    Native.HttpServer.listen portNumber settings

{-|
-}
type alias Settings =
  { onRequest : Request -> Task Never ()
  , onClose : () -> Task Never ()
  }

{-|
-}
reply : Request -> Value -> Task x ()
reply request value =
  Native.HttpServer.reply request (Encode.encode 4 value)

getPath : Request -> String
getPath = Native.HttpServer.getPath

{-|
-}
close : Server -> Task x ()
close =
  Native.HttpServer.close
