module Multitier.Server.HttpServer.LowLevel exposing
  ( listen
  , Settings
  , reply
  , replyFile
  , broadcast
  , close
  , Server
  , Request
  )


import Task exposing (Task)
import Json.Encode as Encode exposing (Value)

import Native.Multitier.Server.HttpServer.LowLevel
import Multitier.Server.HttpServer.Utils exposing (Method)


type Server = Server

type alias Request = { method: Method
                     , path: String
                     , body: String
                     , rawRequest: RawRequest }


type RawRequest = RawRequest

listen : Int -> Settings -> Task x Server
listen portNumber settings =
  Native.Multitier.Server.HttpServer.LowLevel.listen portNumber settings


type alias Settings =
  { onRequest : Request -> Task Never ()
  , onClose : () -> Task Never ()
  }

reply : Request -> Value -> Task x ()
reply request value =
  Native.Multitier.Server.HttpServer.LowLevel.reply request (Encode.encode 4 value)

replyFile : Request -> String -> Task x ()
replyFile request filename =
  Native.Multitier.Server.HttpServer.LowLevel.replyFile request filename

broadcast : String -> Task x ()
broadcast value =
  Native.Multitier.Server.HttpServer.LowLevel.broadcast value

close : Server -> Task x ()
close = Native.Multitier.Server.HttpServer.LowLevel.close
