module Internal.LowLevel exposing (..)

import Native.Internal.LowLevel
import Json.Encode as Json exposing (Value)

toJSON : a -> Value
toJSON = Native.Internal.LowLevel.toJSON

fromJSON : Value -> a
fromJSON = Native.Internal.LowLevel.fromJSON

fromJSONString : String -> a
fromJSONString = Native.Internal.LowLevel.fromJSONString
