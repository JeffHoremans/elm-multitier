module Multitier.Type exposing
  ( Type
  , string
  , int
  , bool
  , float
  , void
  , list
  , array)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Array exposing (Array)

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

list : Type a -> Type (List a)
list (decode,encode) = (Decode.list decode, (\list -> Encode.list (List.map encode list)))

array : Type a -> Type (Array a)
array (decode, encode) = (Decode.array decode, (\array -> Encode.array (Array.map encode array)))
