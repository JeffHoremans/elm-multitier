effect module Multitier.Server.Console where { command = MyCmd }
  exposing ( log)

import Task exposing (Task, andThen)

import Native.Multitier.Server.Console

-- EFFECT MANAGER

type MyCmd msg = Log String

log : String -> Cmd msg
log value = command (Log value)

cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap f (Log value) = Log value

init : Task Never ()
init = Task.succeed ()

onEffects : Platform.Router msg Msg -> List (MyCmd msg) -> () -> Task Never ()
onEffects router cmdList state = case cmdList of
  Log value :: cmds -> Native.Multitier.Server.Console.log value |> Task.andThen (\_ -> onEffects router cmds state)
  [] -> Task.succeed state

-- HANDLE SELF MESSAGES

type Msg = None

onSelfMsg : Platform.Router msg Msg -> Msg -> () -> Task Never ()
onSelfMsg router selfMsg state = Task.succeed state
