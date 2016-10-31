module Multitier.Procedure exposing
  (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Task exposing (Task)

import Multitier.Error exposing (Error(..))
import Multitier.Type exposing (Type)

type alias Identifier = String

type RemoteProcedureCall msg = RPC Identifier (Handlers msg) Arguments

type Arguments =  A0 |
                  A1 Value |
                  A2 Value Value |
                  A3 Value Value Value |
                  A4 Value Value Value Value |
                  A5 Value Value Value Value Value |
                  A6 Value Value Value Value Value Value |
                  A7 Value Value Value Value Value Value Value

type RemoteProcedure = RP Identifier Procedure

rp0: Procedure0 msg -> RemoteProcedure
rp0 (Proc0 identifier _ m0) = RP identifier (P0 m0)

rp1: Procedure1 a msg -> RemoteProcedure
rp1 (Proc1 identifier _ _ m1) = RP identifier (P1 m1)

rp2: Procedure2 a b msg -> RemoteProcedure
rp2 (Proc2 identifier _ _ _ m2) = RP identifier (P2 m2)

rp3: Procedure3 a b c msg -> RemoteProcedure
rp3 (Proc3 identifier _ _ _ _ m3) = RP identifier (P3 m3)

rp4: Procedure4 a b c d msg -> RemoteProcedure
rp4 (Proc4 identifier _ _ _ _ _ m4) = RP identifier (P4 m4)

rp5: Procedure5 a b c d e msg -> RemoteProcedure
rp5 (Proc5 identifier _ _ _ _ _ _ m5) = RP identifier (P5 m5)

rp6: Procedure6 a b c d e f msg -> RemoteProcedure
rp6 (Proc6 identifier _ _ _ _ _ _ _ m6) = RP identifier (P6 m6)

rp7: Procedure7 a b c d e f g msg -> RemoteProcedure
rp7 (Proc7 identifier _ _ _ _ _ _ _ _m7) = RP identifier (P7 m7)

type Procedure = P0 Method0 | P1 Method1 | P2 Method2 | P3 Method3 | P4 Method4 | P5 Method5 | P6 Method6 | P7 Method7

type Procedure0 msg =                 Proc0 Identifier (Handlers msg) Method0
type Procedure1 a msg =               Proc1 Identifier (Type a) (Handlers msg) Method1
type Procedure2 a b msg =             Proc2 Identifier (Type a) (Type b) (Handlers msg) Method2
type Procedure3 a b c msg =           Proc3 Identifier (Type a) (Type b) (Type c) (Handlers msg) Method3
type Procedure4 a b c d msg =         Proc4 Identifier (Type a) (Type b) (Type c) (Type d) (Handlers msg) Method4
type Procedure5 a b c d e msg =       Proc5 Identifier (Type a) (Type b) (Type c) (Type d) (Type e) (Handlers msg) Method5
type Procedure6 a b c d e f msg =     Proc6 Identifier (Type a) (Type b) (Type c) (Type d) (Type e) (Type f) (Handlers msg) Method6
type Procedure7 a b c d e f g msg =   Proc7 Identifier (Type a) (Type b) (Type c) (Type d) (Type e) (Type f) (Type g) (Handlers msg) Method7

type alias Handlers msg = ((Error -> msg),(Value -> msg))

mapHandlers : (a -> msg) -> Handlers a -> Handlers msg
mapHandlers f (onError, onSuccess) = ((onError >> f), (onSuccess >> f))

type alias Method0 = Task Error Value
type alias Method1 = (Value -> Task Error Value)
type alias Method2 = (Value -> Value -> Task Error Value)
type alias Method3 = (Value -> Value -> Value -> Task Error Value)
type alias Method4 = (Value -> Value -> Value -> Value -> Task Error Value)
type alias Method5 = (Value -> Value -> Value -> Value -> Value -> Task Error Value)
type alias Method6 = (Value -> Value -> Value -> Value -> Value -> Value -> Task Error Value)
type alias Method7 = (Value -> Value -> Value -> Value -> Value -> Value -> Value -> Task Error Value)


procedure0 : String -> (Error -> msg) -> (result -> msg) -> Type result -> Task Error result -> Procedure0 msg
procedure0 identifier onError onSucc (decodeResult, encodeResult) task =
  let onSuccess = mapOnSuccess onError onSucc decodeResult
      mappedTask = mapTask0 encodeResult task
    in Proc0 identifier (onError,onSuccess) mappedTask

procedure1 : String -> (Error -> msg) -> (result -> msg) -> Type a -> Type result -> (a -> Task Error result) -> Procedure1 a msg
procedure1 identifier onError onSucc (decodeA, encodeA) (decodeResult, encodeResult) f =
  let onSuccess = mapOnSuccess onError onSucc decodeResult
      mappedTask = mapTask1 decodeA encodeResult f
    in Proc1 identifier (decodeA, encodeA) (onError,onSuccess) mappedTask

mapOnSuccess : (Error -> msg) -> (a -> msg) -> Decoder a -> (Value -> msg)
mapOnSuccess onError onSuccess decoder =
  \value -> let res = Decode.decodeValue decoder value
    in case res of
      Ok result -> onSuccess result
      Err err -> onError (MultitierError err)

mapTask0 : (result -> Value) -> Task Error result -> Task Error Value
mapTask0 encode task = Task.map encode task

mapTask1 : Decoder a -> (result -> Value) -> (a -> Task Error result) -> (Value -> Task Error Value)
mapTask1 decodeA encodeResult taskf = \value ->
  let res = Decode.decodeValue decodeA value
    in Task.map encodeResult (case res of
      Ok a -> (taskf a)
      Err err -> Task.fail (MultitierError "Failed to decode argument"))
