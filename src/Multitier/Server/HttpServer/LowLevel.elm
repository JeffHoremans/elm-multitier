module Multitier.Server.HttpServer.LowLevel exposing
  ( listen
  , EventHandlers
  , reply
  , replyFile
  , createSocketRouter
  , openSocket
  , closeSocket
  , broadcast
  , send
  , Server
  , SocketRouter
  , Socket(..)
  , Request
  , ClientId(..)
  )


import Task exposing (Task)
import Json.Encode as Encode exposing (Value)

import Native.Multitier.Server.HttpServer.LowLevel
import Multitier.Server.HttpServer.Utils exposing (Method)


type Server = Server
type SocketRouter = SocketRouter
type Socket = Socket String

type alias Request = { method: Method
                     , path: String
                     , body: String
                     , rawRequest: RawRequest }

type RawRequest = RawRequest

type ClientId = ClientId String Int

type alias Message = { clientId: ClientId
                     , data: String}

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

reply : Request -> Value -> Task x ()
reply request value = Native.Multitier.Server.HttpServer.LowLevel.reply request (Encode.encode 4 value)

replyFile : Request -> String -> Task x ()
replyFile request filename = Native.Multitier.Server.HttpServer.LowLevel.replyFile request filename

createSocketRouter : Server -> Task x SocketRouter
createSocketRouter server = Native.Multitier.Server.HttpServer.LowLevel.createSocketRouter server

openSocket : SocketRouter -> String -> WebSocketEventHandlers -> Task x Socket
openSocket router path handlers = Native.Multitier.Server.HttpServer.LowLevel.openSocket router path handlers

closeSocket : SocketRouter -> Socket -> Task x ()
closeSocket router socket = Native.Multitier.Server.HttpServer.LowLevel.closeSocket router socket

broadcast : Socket -> String -> Task x ()
broadcast socket message = Native.Multitier.Server.HttpServer.LowLevel.broadcast socket message

send : Socket -> ClientId -> String -> Task x ()
send socket cid message = Native.Multitier.Server.HttpServer.LowLevel.send socket cid message
