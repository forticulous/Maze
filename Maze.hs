module Maze where

import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Data.Lens.Common

import Console
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

readCmd :: String -> Command
readCmd "w" = GoUp
readCmd "s" = GoDown
readCmd "d" = GoRight
readCmd "a" = GoLeft
readCmd "q" = Exit
readCmd _   = Exit

getCmd :: IO Command
getCmd = getLine >>= return . readCmd

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
                    modify $ updateWorld cmd
                    modify $ fogOfWar 

gameEffect :: World -> IO ()
gameEffect wrld = drawWorld wrld 

checkWin :: World -> IO Bool
checkWin wrld = let coord = wrld ^. coordL
                    rows = wrld ^. levelRowsL
                    win = getAtCoord coord rows == '*'
                in if win
                   then putStrLn "You win!" >> return True
                   else return False

gameLoop :: GameState () 
gameLoop = do
           wrld <- get
           lift $ gameEffect wrld
           cmd <- lift getCmd
           updateOneTurn cmd
           wrld <- get
           win <- lift $ checkWin wrld
           unless (cmd == Exit || win) gameLoop

initWorld :: World
initWorld = let allFalse = (map . map) $ \_ -> False
            in fogOfWar $ World (1,1) (Level level (allFalse level))

main :: IO ()
main = evalStateT gameLoop initWorld
