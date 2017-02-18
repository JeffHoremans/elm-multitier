module Multitier.Server.WebSocket
    exposing
        ( ClientId
        , encodecid
        , decodecid
        , listen
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

listen : String -> (ClientId -> msg) -> (ClientId -> msg) -> ((ClientId, String) -> msg) -> Sub msg
listen path onConnect onDisconnect onMessage =
  let safePath = if String.startsWith "/" path then path else ("/" ++ path)
    in HttpServer.listenToSocket safePath onConnect onDisconnect onMessage

broadcast : String -> String -> Cmd msg
broadcast path message = HttpServer.broadcast path message

multicast : String -> List ClientId -> String -> Cmd msg
multicast path ids message = Cmd.batch (List.map (\cid -> send path cid message) ids)

send : String -> ClientId -> String -> Cmd msg
send path uid message = HttpServer.send path uid message
