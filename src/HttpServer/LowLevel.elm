module HttpServer.LowLevel exposing
  ( listen
  , Settings
  , reply
  , replyFile
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

listen : Int -> Settings -> Task x Server
listen portNumber settings =
  Native.HttpServer.listen portNumber settings


type alias Settings =
  { onRequest : Request -> Task Never ()
  , onClose : () -> Task Never ()
  }

reply : Request -> Value -> Task x ()
reply request value =
  Native.HttpServer.reply request (Encode.encode 4 value)

replyFile : Request -> String -> Task x ()
replyFile request filename =
  Native.HttpServer.replyFile request filename

getPath : Request -> String
getPath = Native.HttpServer.getPath

close : Server -> Task x ()
close =
  Native.HttpServer.close
