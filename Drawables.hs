module Drawables where

import Control.Monad (when)
import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

vertify3 :: [(GLfloat, GLfloat, GLfloat)] -> IO ()
vertify3 = sequence_ . (map $ vertex . uncurry3 Vertex3)

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

drawAxes :: IORef Bool -> IO ()
drawAxes showAxes = do 
           s <- get showAxes
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
