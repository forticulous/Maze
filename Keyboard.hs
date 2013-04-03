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
keyboardAct c (Char 'w') Down = do
  c $= GoDown
  postRedisplay Nothing
keyboardAct c (Char 's') Down = do
  c $= GoUp
  postRedisplay Nothing
keyboardAct _ _ _ = return ()

keyboardMouse command key state modifiers position = do
  keyboardAct command key state

handleViewCommands :: Command -> IORef (GLfloat, GLfloat, GLfloat) ->
                      IORef Bool -> IO ()
handleViewCommands ToggleAxes _ showAxes =
    showAxes $~ not
handleViewCommands ZViewDown angle _ =
    angle $= (0.0,0.0,-1.0)
handleViewCommands ZViewUp angle _ =
    angle $= (0.0,0.0,1.0)
handleViewCommands YViewDown angle _ =
    angle $= (5.0,0.0,0.0)
handleViewCommands YViewUp angle _ =
    angle $= (-1.0,0.0,0.0)
handleViewCommands XViewDown angle _ =
    angle $= (0.0,-1.0,0.0)
handleViewCommands XViewUp angle _ =
    angle $= (0.0,1.0,0.0)
handleViewCommands _ angle _ =
    angle $= (0.0,0.0,0.0)
