module World where

import qualified Control.Monad.Trans.State as S
import Control.Monad.Trans.Class (lift)
import Data.Lens.Common
import Graphics.Rendering.OpenGL hiding (Level)
import Graphics.UI.GLUT hiding (Level)

import Drawables
import Types

type CoordState a = S.StateT Coord IO a

blockSize :: GLfloat
blockSize = 0.1

drawFromChar :: Char -> IO ()
drawFromChar '#' = brownWall blockSize
drawFromChar '*' = goal blockSize
drawFromChar _   = return ()

canSee :: Char -> Bool -> Bool
canSee ' ' _ = False
canSee _ vis = vis

drawSquare :: (Char, Bool) -> CoordState ()
drawSquare (chr, vis) = do 
                        if canSee chr vis
                        then lift $ drawFromChar chr
                        else return ()
                        cursorForward 1

drawRow :: [(Char, Bool)] -> CoordState ()
drawRow row = do
              mapM_ drawSquare row
              cursorDownLine 1
              setCursorColumn 0

zipLevel :: Level -> [[(Char, Bool)]]
zipLevel lvl = let rows = lvl ^. rowsL
                   visRows = lvl ^. visibleL
               in [zip (rows !! i) (visRows !! i) | i <- [0..(length rows - 1)]]

drawLevel :: Level -> CoordState ()
drawLevel lvl = mapM_ drawRow $ zipLevel lvl

drawWorld :: World -> IO ()
drawWorld wrld = (flip S.evalStateT) (wrld ^. coordL) $ do
                   setCursorPosition 0 0
                   drawLevel $ wrld ^. levelL

drawPlayer :: Coord -> IO ()
drawPlayer coord = player blockSize

setCursorPosition :: Int -> Int -> CoordState ()
setCursorPosition x y = do
                        (_, oldY) <- S.get 
                        cursorDownLine (y - oldY)
                        setCursorColumn x

cursorForward :: Int -> CoordState ()
cursorForward n = do
                  lift . translate $ Vector3 (blockSize * (realToFrac (2*n))) 0.0 0.0
                  S.modify $ (fstLens ^%= (+n))

cursorDownLine :: Int -> CoordState ()
cursorDownLine n = do
                   lift . translate $ Vector3 0.0 0.0 (blockSize * (realToFrac (2*n)))
                   S.modify $ (sndLens ^%= (+n))

setCursorColumn :: Int -> CoordState ()
setCursorColumn newX = do 
                       (oldX, _) <- S.get
                       cursorForward (newX - oldX)
