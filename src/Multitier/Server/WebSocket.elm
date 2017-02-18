module Multitier.Server.WebSocket
    exposing
        ( ClientId
        , encodecid
        , decodecid
        , listen
        , listenAndMonitor
        , keepAlive
        , keepAliveAndMonitor
        , broadcast
        , multicast
        , send
        )

import String
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)

import Multitier.Server.HttpServer as HttpServer

type alias ClientId = HttpServer.ClientId

encodecid : ClientId -> Value
encodecid = HttpServer.encodecid

decodecid : Decoder ClientId
decodecid = HttpServer.decodecid

listen : String -> ((ClientId, String) -> msg) -> Sub msg
listen path onMessage = listenToSocket path Nothing Nothing (Just onMessage)

listenAndMonitor : String -> (ClientId -> msg) -> (ClientId -> msg) -> ((ClientId, String) -> msg) -> Sub msg
listenAndMonitor path onConnect onDisconnect onMessage = listenToSocket path (Just onConnect) (Just onDisconnect) (Just onMessage)

keepAlive : String -> Sub msg
keepAlive path = listenToSocket path Nothing Nothing Nothing

keepAliveAndMonitor : String -> (ClientId -> msg) -> (ClientId -> msg) -> Sub msg
keepAliveAndMonitor path onConnect onDisconnect = listenToSocket path (Just onConnect) (Just onDisconnect) Nothing

listenToSocket : String -> Maybe (ClientId -> msg) -> Maybe (ClientId -> msg) -> Maybe ((ClientId, String) -> msg) -> Sub msg
listenToSocket path onConnect onDisconnect onMessage = HttpServer.listenToSocket (safePath path) onConnect onDisconnect onMessage

broadcast : String -> String -> Cmd msg
broadcast path message = HttpServer.broadcast (safePath path) message

multicast : String -> List ClientId -> String -> Cmd msg
multicast path ids message = Cmd.batch (List.map (\cid -> send path cid message) ids)

send : String -> ClientId -> String -> Cmd msg
send path uid message = HttpServer.send (safePath path) uid message

safePath : String -> String
safePath path = if String.startsWith "/" path then path else ("/" ++ path)
