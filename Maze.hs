module Maze where

import Control.Monad (when, unless)
import Control.Monad.Trans.Class (lift)
import Data.IORef
import qualified Control.Monad.Trans.State as S
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Data.Lens.Common

import World
import Drawables
import Keyboard
import Types

getAtCoord :: Coord -> [String] -> Char
getAtCoord coord rows = (rows !! (coord ^. sndLens)) !! (coord ^. fstLens)

safeReplaceAt :: Int -> a -> [a] -> [a]
safeReplaceAt _ _      [] = []
safeReplaceAt n newVal xs = 
  if n >= 0 && n < length xs
  then replaceAt n newVal xs
  else xs

replaceAt :: Int -> a -> [a] -> [a]
replaceAt n newVal (x:xs)
  | n == 0    = newVal:xs
  | otherwise = x:replaceAt (n - 1) newVal xs

visibleAtCoord :: Coord -> [[Bool]] -> [[Bool]]
visibleAtCoord coord vis = let x = coord ^. fstLens
                               y = coord ^. sndLens
                           in safeReplaceAt y (safeReplaceAt x True (vis !! y)) vis

canMove :: Char -> Bool
canMove '#' = False
canMove _   = True

move :: Command -> Coord -> Coord
move GoUp    = \(x, y) -> (x, y - 1)
move GoDown  = \(x, y) -> (x, y + 1)
move GoRight = \(x, y) -> (x + 1, y)
move GoLeft  = \(x, y) -> (x - 1, y)
move _       = id

updateWorld :: Command -> World -> World
updateWorld cmd wrld = let rows     = wrld ^. levelRowsL
                           newCoord = move cmd $ wrld ^. coordL
                       in if canMove $ getAtCoord newCoord rows
                          then coordL ^%= move cmd $ wrld
                          else wrld

visibleComp :: Coord -> [[Bool]] -> [[Bool]]
visibleComp coord = visibleAtCoord (move GoUp $ coord) . 
                    visibleAtCoord (move GoDown $ coord) .
                    visibleAtCoord (move GoLeft $ coord) .
                    visibleAtCoord (move GoRight $ coord) .
                    visibleAtCoord (move GoLeft . move GoUp $ coord) .
                    visibleAtCoord (move GoLeft . move GoDown $ coord) .
                    visibleAtCoord (move GoRight . move GoUp $ coord) .
                    visibleAtCoord (move GoRight . move GoDown $ coord)
                           
fogOfWar :: World -> World
fogOfWar wrld = let coord = wrld ^. coordL
                in levelVisibleL ^%= visibleComp coord $ wrld

updateOneTurn :: Command -> GameState ()
updateOneTurn cmd = do 
                    S.modify $ updateWorld cmd
                    S.modify $ fogOfWar 

gameEffect :: World -> IO ()
gameEffect wrld = do 
                  preservingMatrix $ drawWorld wrld
                  drawPlayer $ wrld ^. coordL

checkWin :: World -> IO Bool
checkWin wrld = let coord = wrld ^. coordL
                    rows = wrld ^. levelRowsL
                    win = getAtCoord coord rows == '*'
                in if win
                   then putStrLn "You win!" >> return True
                   else return False

emptyVector :: GLfloat -> GLfloat -> GLfloat -> Bool
emptyVector x y z = all (== 0.0) [x, y, z]

--gameLoop :: GameState () 
--gameLoop = do
--           wrld <- get
--           lift $ gameEffect wrld
--           cmd <- lift getCmd
--           updateOneTurn cmd
--           wrld <- get
--           win <- lift $ checkWin wrld
--           unless (cmd == Exit || win) gameLoop

display :: IORef (GLfloat, GLfloat, GLfloat) -> IORef Bool -> 
           IORef Command -> IORef World -> IO ()
display angle showAxes command world = do
           clear [ColorBuffer,DepthBuffer]
           wrld <- get world
           cmd <- get command
           handleViewCommands cmd angle showAxes
           (ax, ay, az) <- get angle
           when (not $ emptyVector ax ay az) $
             rotate 10 $ Vector3 ax ay az
           (flip S.evalStateT) wrld $ do
             updateOneTurn cmd
             newWrld <- S.get
             lift (world $= newWrld)
           wrld <- get world
           gameEffect wrld
           drawAxes showAxes
           swapBuffers 

initWorld :: World
initWorld = let allFalse = (map . map) $ \_ -> False
            in fogOfWar $ World (1,1) (Level level (allFalse level))

main :: IO ()
main = do
       (programName, _) <- getArgsAndInitialize
       initialDisplayMode $= [WithDepthBuffer,DoubleBuffered]
       createWindow "Maze"
       depthFunc $= Just Less
       angle <- newIORef (0.0::GLfloat, 0.0::GLfloat, 0.0::GLfloat)
       showAxes <- newIORef False
       world <- newIORef initWorld
       command <- newIORef NoOp
       -- initial rotation
       rotate 50 $ Vector3 (-1.0::GLfloat) 0.0 0.0
       rotate 50 $ Vector3 (0.0::GLfloat) 1.0 0.0
       keyboardMouseCallback $= Just (keyboardMouse command)
       displayCallback $= display angle showAxes command world
       mainLoop
