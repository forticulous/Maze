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

display :: IORef (GLfloat, GLfloat, GLfloat) -> IORef (GLfloat, GLfloat) -> IORef Bool -> IO ()
display angle position showAxis = do
              clear [ColorBuffer]
              (ax, ay, az) <- get angle
              when (not $ emptyVector ax ay az) $
                rotate 10 $ Vector3 ax ay az
              preservingMatrix $ do
                brownWall (0.1::GLfloat)
                translate $ Vector3 (0.2::GLfloat) 0.0 0.0
                brownWall (0.1::GLfloat)
                translate $ Vector3 (0.2::GLfloat) 0.0 0.0
                goal (0.1::GLfloat)
                translate $ Vector3 (0.2::GLfloat) 0.0 0.0
                player (0.1::GLfloat)
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

emptyVector :: GLfloat -> GLfloat -> GLfloat -> Bool
emptyVector x y z = all (== 0.0) [x, y, z]

square :: PrimitiveMode -> GLfloat -> IO ()
square mode w = renderPrimitive mode $ vertify3
             [ ( w, 0, w), ( w, 0,-w),
               (-w, 0,-w), (-w, 0, w) ]

squareX :: PrimitiveMode -> GLfloat -> GLfloat -> IO ()
squareX mode x w = preservingMatrix $ do
                     rotate 90 $ Vector3 (0.0::GLfloat) 0.0 1.0
                     translate $ Vector3 0.0 x 0.0
                     square mode w

squareY :: PrimitiveMode -> GLfloat -> GLfloat -> IO ()
squareY mode y w = preservingMatrix $ do
                     translate $ Vector3 0.0 y 0.0
                     square mode w

squareZ :: PrimitiveMode -> GLfloat -> GLfloat -> IO ()
squareZ mode z w = preservingMatrix $ do
                     rotate 90 $ Vector3 (1.0::GLfloat) 0.0 0.0
                     translate $ Vector3 0.0 z 0.0
                     square mode w

brownWall :: GLfloat -> IO ()
brownWall w = do
              color $ Color3 (0.721568::GLfloat) 0.541176 0.0
              wall w 

greenWall :: GLfloat -> IO ()
greenWall w = do
              color $ Color3 (0.0::GLfloat) 1.0 0.0
              wall w

wall :: GLfloat -> IO ()
wall w = do 
         squareX Quads w w
         squareX Quads (-w) w
         squareY Quads w w
         squareY Quads (-w) w
         squareZ Quads w w
         squareZ Quads (-w) w
         color $ Color3 (1.0::GLfloat) 1.0 1.0
         sequence_ $ 
           [ squareX LineLoop w w
           , squareX LineLoop (-w) w
           , squareY LineLoop w w
           , squareY LineLoop (-w) w
           , squareZ LineLoop w w
           , squareZ LineLoop (-w) w ]

player :: GLfloat -> IO ()
player w = do
           preservingMatrix $ do
             translate $ Vector3 0.0 (2*w/3) 0.0
             greenWall (w/3)
           preservingMatrix $ do
             translate $ Vector3 0.0 (w/9) 0.0
             greenWall (2*w/9)
           preservingMatrix $ do
             translate $ Vector3 0.0 (-w/3) 0.0
             greenWall (2*w/9)
           preservingMatrix $ do
             translate $ Vector3 0.0 (-7*w/9) 0.0
             rotate 90 $ Vector3 (1.0::GLfloat) 0.0 0.0
             preservingMatrix $ do
               translate $ Vector3 0.0 (w/3) 0.0
               greenWall (2*w/9)
             preservingMatrix $ do
               translate $ Vector3 0.0 (-w/3) 0.0
               greenWall (2*w/9)
           preservingMatrix $ do
             rotate 90 $ Vector3 (1.0::GLfloat) 0.0 0.0
             preservingMatrix $ do
               translate $ Vector3 0.0 (4*w/9) 0.0
               greenWall (2*w/9)
             preservingMatrix $ do
               translate $ Vector3 0.0 (-4*w/9) 0.0
               greenWall (2*w/9)

goal :: GLfloat -> IO ()
goal w = do
         color $ Color3 (1.0::GLfloat) 1.0 0.0
         squareY Quads (-w) w
         renderPrimitive Triangles $ vertify3
           [ ( 0, w, 0), ( w,-w, w), ( w,-w,-w)
           , ( 0, w, 0), ( w,-w,-w), (-w,-w,-w)
           , ( 0, w, 0), (-w,-w,-w), (-w,-w, w)
           , ( 0, w, 0), (-w,-w, w), ( w,-w, w) ]
         color $ Color3 (1.0::GLfloat) 1.0 1.0
         squareY LineLoop (-w) w
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
