module Multitier.Server.WebSocket
    exposing
        ( SocketServer
        , ClientId
        , listen
        , broadcast
        , multicast
        , send
        )

import Multitier.Server.HttpServer as HttpServer

type alias SocketServer = HttpServer.SocketServer
type alias ClientId = HttpServer.ClientId

listen : (SocketServer -> msg) -> (ClientId -> msg) -> (ClientId -> msg) -> ((ClientId, String) -> msg) -> Sub msg
listen onSocketOpen onConnect onDisconnect onMessage = HttpServer.listenToSocket onSocketOpen onConnect onDisconnect onMessage

broadcast : SocketServer -> String -> Cmd msg
broadcast server message = HttpServer.broadcast server message

multicast : SocketServer -> List ClientId -> String -> Cmd msg
multicast server ids message = Cmd.batch (List.map (\cid -> send server cid message) ids)

send : SocketServer -> ClientId -> String -> Cmd msg
send server uid message = HttpServer.send server uid message
