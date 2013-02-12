module Types where

import Control.Monad.Trans.State
import Data.Lens.Common
import qualified Control.Category as Cat

data Command = GoUp | GoDown | GoRight | GoLeft | Exit deriving (Eq)

type Coord = (Int, Int)
type GameState a = StateT World IO a
data Level = Level 
             { getRows :: [String]
             , getVisible :: [[Bool]] }

data World = World
             { getCoord :: Coord
             , getLevel :: Level }

level :: [String]
level = [ "#########################"
        , "#     #        #      * #"
        , "###   ######   #  #######"
        , "#        #        #     #"
        , "#  ###   ######   #     #"
        , "#  #  #           #  #  #"
        , "#  #  #  ###   #  #  #  #"
        , "#  #        #  #  #  #  #"
        , "#  ######   ##########  #"
        , "#  #                    #"
        , "#########################" ]

-- lenses
setCoord :: Coord -> World -> World
setCoord coord wrld = wrld { getCoord = coord }

coordL :: Lens World Coord
coordL = lens getCoord setCoord

setLevel :: Level -> World -> World
setLevel lvl wrld = wrld { getLevel = lvl }

levelL :: Lens World Level
levelL = lens getLevel setLevel

setRows :: [String] -> Level -> Level
setRows rows lvl = lvl { getRows = rows }

rowsL :: Lens Level [String]
rowsL = lens getRows setRows

setVisible :: [[Bool]] -> Level -> Level
setVisible vis lvl = lvl { getVisible = vis }

visibleL :: Lens Level [[Bool]]
visibleL = lens getVisible setVisible

levelRowsL :: Lens World [String]
levelRowsL = rowsL Cat.. levelL

levelVisibleL :: Lens World [[Bool]]
levelVisibleL = visibleL Cat.. levelL
