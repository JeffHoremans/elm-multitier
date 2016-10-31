module HttpServer.LowLevel exposing
  ( listen
  , Settings
  , reply
  , replyFile
  , close
  , Server
  , Request
  )


import Task exposing (Task)
import Json.Encode as Encode exposing (Value)

import Native.HttpServer.LowLevel
import HttpServer.Utils exposing (Method)


type Server = Server

type alias Request = { method: Method
                     , path: String
                     , body: String
                     , rawRequest: RawRequest }


type RawRequest = RawRequest

listen : Int -> Settings -> Task x Server
listen portNumber settings =
  Native.HttpServer.LowLevel.listen portNumber settings


type alias Settings =
  { onRequest : Request -> Task Never ()
  , onClose : () -> Task Never ()
  }

reply : Request -> Value -> Task x ()
reply request value =
  Native.HttpServer.LowLevel.reply request (Encode.encode 4 value)

replyFile : Request -> String -> Task x ()
replyFile request filename =
  Native.HttpServer.LowLevel.replyFile request filename
--
-- getPath : Request -> String
-- getPath = Native.HttpServer.LowLevel.getPath
--
-- getMethod : Request -> String
-- getMethod = Native.HttpServer.LowLevel.getMethod
--
-- getBody : Request -> Value
-- getBody = Native.HttpServer.LowLevel.getBody

close : Server -> Task x ()
close = Native.HttpServer.LowLevel.close
