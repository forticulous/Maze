module OpenGLUT where --(
    --main,
    --drawWorld
    --) where

import Control.Monad (when)
import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Types

main :: IO ()
main = drawWorld

drawWorld :: IO ()
drawWorld = do
            (programName, _) <- getArgsAndInitialize
            initialDisplayMode $= [DoubleBuffered]
            createWindow "Maze"
            angle <- newIORef (0.0::GLfloat, 0.0::GLfloat, 0.0::GLfloat)
            position <- newIORef (0.0::GLfloat, 0.0)
            showAxis <- newIORef False
            keyboardMouseCallback $= Just (keyboardMouse angle position showAxis)
            displayCallback $= display angle position showAxis
            mainLoop

--display :: IORef GLfloat -> IORef GLfloat -> IORef Bool -> IO ()
display angle position showAxis = do
              clear [ColorBuffer]
              (ax, ay, az) <- get angle
              when (any (/= (0.0::GLfloat)) [ax, ay, az]) $
                rotate 10 $ Vector3 ax ay az
              preservingMatrix $ do
                wall (0.1::GLfloat)
                translate $ Vector3 (0.2::GLfloat) 0.0 0.0
                wall (0.1::GLfloat)
                translate $ Vector3 (0.2::GLfloat) 0.0 0.0
                goal (0.1::GLfloat)
              drawAxis showAxis
              swapBuffers

keyboardAct a p s (Char ' ') Down = do
  s' <- get s
  s $= not s'
  a $= (0.0, 0.0, 0.0)
  postRedisplay Nothing
keyboardAct a p s (Char '-') Down = do
  a $= (0.0, 0.0,-1.0)
  postRedisplay Nothing
keyboardAct a p s (Char '=') Down = do
  a $= (0.0, 0.0, 1.0)
  postRedisplay Nothing
keyboardAct a p s (SpecialKey KeyLeft) Down = do
  a $= (0.0, -1.0, 0.0)
  postRedisplay Nothing
keyboardAct a p s (SpecialKey KeyRight) Down = do
  a $= (0.0, 1.0, 0.0)
  postRedisplay Nothing
keyboardAct a p s (SpecialKey KeyDown) Down = do
  a $= (1.0, 0.0, 0.0)
  postRedisplay Nothing
keyboardAct a p s (SpecialKey KeyUp) Down = do
  a $= (-1.0, 0.0, 0.0)
  postRedisplay Nothing
keyboardAct _ _ _ _ _ = return ()

keyboardMouse angle pos axis key state modifiers position = do
  keyboardAct angle pos axis key state

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = ((f a $ b) $ c)

vertify3 :: [(GLfloat, GLfloat, GLfloat)] -> IO ()
vertify3 = sequence_ . (map $ vertex . uncurry3 Vertex3)

square :: GLfloat -> IO ()
square w = vertify3
             [ ( w, 0, w), ( w, 0,-w),
               (-w, 0,-w), (-w, 0, w) ]

squareX :: GLfloat -> GLfloat -> IO ()
squareX x w = vertify3
                [ ( x, w, w), ( x, w,-w), 
                  ( x,-w,-w), ( x,-w, w) ]

squareY :: GLfloat -> GLfloat -> IO ()
squareY y w = vertify3
                [ ( w, y, w), ( w, y,-w), 
                  (-w, y,-w), (-w, y, w) ]

squareZ :: GLfloat -> GLfloat -> IO ()
squareZ z w = vertify3
                [ ( w, w, z), ( w,-w, z), 
                  (-w,-w, z), (-w, w, z) ]

wall :: GLfloat -> IO ()
wall w = do 
         renderPrimitive Quads $ do
           color $ Color3 (0.721568::GLfloat) 0.541176 0.0
           squareX w w
           squareX (-w) w
           squareY w w
           squareY (-w) w
           squareZ w w
           squareZ (-w) w
           return ()
         color $ Color3 (1.0::GLfloat) 1.0 1.0
         sequence_ $ map (renderPrimitive LineLoop) 
           [ squareX w w
           , squareX (-w) w
           , squareY w w
           , squareY (-w) w
           , squareZ w w
           , squareZ (-w) w ]

goal :: GLfloat -> IO ()
goal w = do
         color $ Color3 (1.0::GLfloat) 1.0 0.0
         renderPrimitive Quads $ squareY (-w) w
         renderPrimitive Triangles $ vertify3
           [ ( 0, w, 0), ( w,-w, w), ( w,-w,-w)
           , ( 0, w, 0), ( w,-w,-w), (-w,-w,-w)
           , ( 0, w, 0), (-w,-w,-w), (-w,-w, w)
           , ( 0, w, 0), (-w,-w, w), ( w,-w, w) ]
         color $ Color3 (1.0::GLfloat) 1.0 1.0
         renderPrimitive LineLoop $ squareY (-w) w
         sequence_ $ map (renderPrimitive LineLoop . vertify3)
           [ [ ( 0, w, 0), ( w,-w, w), ( w,-w,-w) ]
           , [ ( 0, w, 0), ( w,-w,-w), (-w,-w,-w) ]
           , [ ( 0, w, 0), (-w,-w,-w), (-w,-w, w) ]
           , [ ( 0, w, 0), (-w,-w, w), ( w,-w, w) ] ]

--drawAxis :: IORef Bool -> IO ()
drawAxis showAxis = do 
           s <- get showAxis
           when s $ renderPrimitive Lines $ do
             color $ Color3 (1.0::GLfloat) 0.0 0.0
             vertify3 
               [ (0.0::GLfloat, 0.0, 0.0), (1.0, 0.0, 0.0) ]
             color $ Color3 (0.0::GLfloat) 1.0 0.0
             vertify3 
               [ (0.0::GLfloat, 0.0, 0.0), (0.0, 1.0, 0.0) ]
             color $ Color3 (0.0::GLfloat) 0.0 1.0
             vertify3 
               [ (0.0::GLfloat, 0.0, 0.0), (0.0, 0.0, 1.0) ]
