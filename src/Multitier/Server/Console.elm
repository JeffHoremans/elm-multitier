module Multitier.Server.Console exposing (..)

import Task exposing (Task, andThen)

import Native.Multitier.Server.Console

log : value -> Task never ()
log value = (Native.Multitier.Server.Console.log value)
