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
--canSee ' ' _ = False
--canSee _ vis = vis
canSee _ _ = True

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
drawWorld wrld = (flip S.evalStateT) (0,0) $ do
                   setCursorPosition 0 0
                   drawLevel $ wrld ^. levelL
                   drawPlayer $ wrld ^. coordL

drawPlayer :: Coord -> CoordState ()
drawPlayer coord = do
                   uncurry (flip setCursorPosition) coord
                   lift $ player blockSize

setCursorPosition :: Int -> Int -> CoordState ()
setCursorPosition x y = do
                        (_, oldY) <- S.get 
                        cursorDownLine (y - oldY)
                        setCursorColumn y

cursorForward :: Int -> CoordState ()
cursorForward n = do
                  lift . translate $ Vector3 (blockSize * (realToFrac n)) 0.0 0.0
                  S.modify $ (\coord -> fstLens ^%= (+n) $ coord)

cursorDownLine :: Int -> CoordState ()
cursorDownLine n = do
                   lift . translate $ Vector3 0.0 0.0 (blockSize * (realToFrac n))
                   S.modify $ (\coord -> sndLens ^%= (+n) $ coord)

setCursorColumn :: Int -> CoordState ()
setCursorColumn newX = do 
                       (oldX, _) <- S.get
                       cursorForward (newX - oldX)
