module Keyboard where

import Data.IORef
import Graphics.UI.GLUT

import Types

keyboardAct c (Char ' ') Down = do
  c $= ToggleAxes
  postRedisplay Nothing
keyboardAct c (Char '-') Down = do
  c $= ZViewDown
  postRedisplay Nothing
keyboardAct c (Char '=') Down = do
  c $= ZViewUp
  postRedisplay Nothing
keyboardAct c (SpecialKey KeyLeft) Down = do
  c $= XViewDown
  postRedisplay Nothing
keyboardAct c (SpecialKey KeyRight) Down = do
  c $= XViewUp
  postRedisplay Nothing
keyboardAct c (SpecialKey KeyDown) Down = do
  c $= YViewDown
  postRedisplay Nothing
keyboardAct c (SpecialKey KeyUp) Down = do
  c $= YViewUp
  postRedisplay Nothing
keyboardAct c (Char 'a') Down = do
  c $= GoLeft
  postRedisplay Nothing
keyboardAct c (Char 'd') Down = do
  c $= GoRight
  postRedisplay Nothing
keyboardAct c (Char 's') Down = do
  c $= GoDown
  postRedisplay Nothing
keyboardAct c (Char 'w') Down = do
  c $= GoUp
  postRedisplay Nothing
keyboardAct _ _ _ = return ()

keyboardMouse command key state modifiers position = do
  keyboardAct command key state
