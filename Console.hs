module Console
    ( drawWorld 
    ) where

import Data.Lens.Common
import System.Console.ANSI

import Types

canSee :: Char -> Bool -> Bool
canSee ' ' _   = False
canSee _   vis = vis

drawSquare :: (Char, Bool) -> IO ()
drawSquare (chr, vis) = if canSee chr vis
                        then putChar chr
                        else cursorForward 1

drawRow :: [(Char, Bool)] -> IO ()
drawRow row = do
              mapM_ drawSquare row
              cursorDownLine 1
              setCursorColumn 0

zipLevel :: Level -> [[(Char, Bool)]]
zipLevel lvl = let rows    = lvl ^. rowsL
                   visRows = lvl ^. visibleL
               in [zip (rows !! i) (visRows !! i) | i <- [0..(length rows - 1)]]

drawLevel :: Level -> IO ()
drawLevel lvl = mapM_ drawRow $ zipLevel lvl

drawWorld :: World -> IO ()
drawWorld wrld = do
                 clearScreen
                 setCursorPosition 0 0
                 drawLevel $ wrld ^. levelL
                 drawPlayer $ wrld ^. coordL
                 cursorToInput

drawPlayer :: Coord -> IO ()
drawPlayer coord = do
                   uncurry (flip setCursorPosition) coord
                   putChar 'P'

cursorToInput :: IO ()
cursorToInput = setCursorPosition 20 0
