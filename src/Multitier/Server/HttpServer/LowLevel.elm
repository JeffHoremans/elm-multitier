module Multitier.Server.HttpServer.LowLevel exposing
  ( listen
  , openSocket
  , EventHandlers
  , reply
  , replyFile
  , broadcast
  , send
  , Server
  , SocketServer
  , Request
  , ClientId
  )


import Task exposing (Task)
import Json.Encode as Encode exposing (Value)

import Native.Multitier.Server.HttpServer.LowLevel
import Multitier.Server.HttpServer.Utils exposing (Method)


type Server = Server
type SocketServer = SocketServer

type alias Request = { method: Method
                     , path: String
                     , body: String
                     , rawRequest: RawRequest }
type RawRequest = RawRequest

type alias ClientId = Int
type alias Message = { clientId: ClientId
                     , data: Value}

type alias WebSocketEventHandlers =
  { onConnect : ClientId -> Task Never ()
  , onDisconnect : ClientId -> Task Never ()
  , onMessage : Message -> Task Never () }

type alias EventHandlers =
  { onRequest : Request -> Task Never ()
  , onClose : () -> Task Never ()
  }

listen : Int -> EventHandlers -> Task x Server
listen portNumber handlers = Native.Multitier.Server.HttpServer.LowLevel.listen portNumber handlers

openSocket : Server -> WebSocketEventHandlers -> Task x SocketServer
openSocket handlers = Native.Multitier.Server.HttpServer.LowLevel.openSocket handlers

reply : Request -> Value -> Task x ()
reply request value = Native.Multitier.Server.HttpServer.LowLevel.reply request (Encode.encode 4 value)

replyFile : Request -> String -> Task x ()
replyFile request filename = Native.Multitier.Server.HttpServer.LowLevel.replyFile request filename

broadcast : SocketServer -> String -> Task x ()
broadcast server message = Native.Multitier.Server.HttpServer.LowLevel.broadcast server message

send : SocketServer -> ClientId -> String -> Task x ()
send server cid message = Native.Multitier.Server.HttpServer.LowLevel.send server cid message
