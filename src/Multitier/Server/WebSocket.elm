module Multitier.Server.WebSocket
    exposing
        ( WebSocket
        , ClientId
        , listen
        , broadcast
        , multicast
        , send
        )

import Multitier.Server.HttpServer as HttpServer
import String

type alias WebSocket = HttpServer.Socket
type alias ClientId = HttpServer.ClientId

listen : String -> (WebSocket -> msg) -> (ClientId -> msg) -> (ClientId -> msg) -> ((ClientId, String) -> msg) -> Sub msg
listen path onSocketOpen onConnect onDisconnect onMessage =
  let safePath = if String.startsWith "/" path then path else ("/" ++ path)
    in HttpServer.listenToSocket path onSocketOpen onConnect onDisconnect onMessage

broadcast : WebSocket -> String -> Cmd msg
broadcast server message = HttpServer.broadcast server message

multicast : WebSocket -> List ClientId -> String -> Cmd msg
multicast server ids message = Cmd.batch (List.map (\cid -> send server cid message) ids)

send : WebSocket -> ClientId -> String -> Cmd msg
send server uid message = HttpServer.send server uid message
