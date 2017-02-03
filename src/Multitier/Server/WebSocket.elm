module Multitier.Server.WebSocket
    exposing
        ( SocketServer
        , ClientId
        , listen
        , broadcast
        , multicast
        , send
        )

import Json.Encode exposing (Value,encode)

import Multitier.Server.HttpServer as HttpServer

type alias SocketServer = HttpServer.SocketServer
type alias ClientId = HttpServer.ClientId

listen : (SocketServer -> msg) -> (ClientId -> msg) -> (ClientId -> msg) -> ((ClientId, Value) -> msg) -> Sub msg
listen onSocketOpen onConnect onDisconnect onMessage = HttpServer.listenToSocket onSocketOpen onConnect onDisconnect onMessage

broadcast : SocketServer -> Value -> Cmd msg
broadcast server message = HttpServer.broadcast server (encode 0 message)

multicast : SocketServer -> List ClientId -> Value -> Cmd msg
multicast server ids message = Cmd.batch (List.map (\cid -> send server cid message) ids)

send : SocketServer -> ClientId -> Value -> Cmd msg
send server uid message = HttpServer.send server uid (encode 0 message)
