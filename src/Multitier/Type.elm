module Multitier.Type exposing
  ( Type
  , string
  , int
  , bool
  , float
  , void)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)

encodeVoid : (() -> Value)
encodeVoid = always Encode.null

decodeVoid : Decoder ()
decodeVoid = Decode.succeed ()

type alias Type a = (Decoder a, (a -> Value))

string : Type String
string = (Decode.string, Encode.string)

int : Type Int
int = (Decode.int, Encode.int)

bool : Type Bool
bool = (Decode.bool, Encode.bool)

float : Type Float
float = (Decode.float, Encode.float)

void : Type ()
void = (decodeVoid, encodeVoid)
