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
  , Request
  , ClientId(..)
  , encodecid
  , decodecid
  )


import Task exposing (Task)
import Json.Encode as Encode exposing (Value, object)
import Json.Decode as Decode exposing (Decoder, map2, field)

import Native.Multitier.Server.HttpServer.LowLevel
import Multitier.Server.HttpServer.Utils exposing (Method)


type Server = Server
type SocketRouter = SocketRouter

type alias Request = { method: Method
                     , path: String
                     , body: String
                     , rawRequest: RawRequest }

type RawRequest = RawRequest

type ClientId = ClientId String Int

encodecid : ClientId -> Value
encodecid (ClientId path id) = object
  [ ("path", Encode.string path)
  , ("id", Encode.int id)]

decodecid : Decoder ClientId
decodecid = map2 ClientId
  (field "path" Decode.string)
  (field "id" Decode.int)

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

openSocket : SocketRouter -> String -> WebSocketEventHandlers -> Task x ()
openSocket router path handlers = Native.Multitier.Server.HttpServer.LowLevel.openSocket router path handlers

closeSocket : SocketRouter -> String -> Task x ()
closeSocket router path = Native.Multitier.Server.HttpServer.LowLevel.closeSocket router path

broadcast : String -> String -> Task x ()
broadcast path message = Native.Multitier.Server.HttpServer.LowLevel.broadcast path message

send : String -> ClientId -> String -> Task x ()
send path cid message = Native.Multitier.Server.HttpServer.LowLevel.send path cid message
