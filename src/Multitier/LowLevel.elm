module Multitier.LowLevel exposing (..)

import Native.Multitier.LowLevel
import Json.Encode as Json exposing (Value)

toJSON : a -> Value
toJSON = Native.Multitier.LowLevel.toJSON

fromJSON : Value -> a
fromJSON = Native.Multitier.LowLevel.fromJSON

fromJSONString : String -> a
fromJSONString = Native.Multitier.LowLevel.fromJSONString

bootstrapStub : serverModel -> serverModel
bootstrapStub = Native.Multitier.LowLevel.bootstrapStub
