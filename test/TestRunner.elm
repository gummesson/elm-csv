module Main where

import Tests

import ElmTest exposing (consoleRunner)
import Console exposing (IO, run)

import Signal exposing (Signal)
import Task


console : IO ()
console =
  consoleRunner Tests.all


port runner : Signal (Task.Task x ())
port runner =
  run console
