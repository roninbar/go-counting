module Counting
  ( Color(..)
  , territories
  , territoryFor
  ) where

import           Control.Arrow       (first, second)
import           Control.Monad.State (State, execState, gets, modify, when)
import           Data.Functor        ((<&>))
import           Data.List           (nub)
import           Data.Maybe          (mapMaybe)
import           Data.Set            (Set)
import qualified Data.Set            as Set

data Color
  = Black
  | White
  deriving (Eq, Ord, Show)

type Coord = (Int, Int)

territories :: [String] -> [(Set Coord, Maybe Color)]
territories board =
  let m = length board
      n = length (head board)
      unoccupied =
        [ (j + 1, i + 1)
        | i <- [0 .. m - 1]
        , j <- [0 .. n - 1]
        , board !! i !! j == ' '
        ]
   in nub $ mapMaybe (territoryFor board) unoccupied

territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board (i, j) =
  let i' = j - 1
      j' = i - 1
   in if 0 <= i' && i' < m && 0 <= j' && j' < n && board !! i' !! j' == ' '
        then Just $
             let (ts, bs) = execState (visit (i', j')) (mempty, mempty)
                 boundaryColors =
                   Set.fromList $
                   [board !! i'' !! j'' | (i'', j'') <- Set.toList bs] <&>
                   flip lookup [('B', Black), ('W', White)]
              in ( Set.map (\(i'', j'') -> (j'' + 1, i'' + 1)) ts
                 , if 1 == Set.size boundaryColors
                     then head $ Set.toList boundaryColors
                     else Nothing)
        else Nothing
  where
    m = length board
    n = length (head board)
    visit :: (Int, Int) -> State (Set Coord, Set Coord) ()
    visit (i', j') = do
      modify $ first $ Set.insert (i', j')
      ts <- gets fst
      let neighbors =
            [ p
            | p@(i'', j'') <-
                [(i' - 1, j'), (i', j' + 1), (i' + 1, j'), (i', j' - 1)]
            , 0 <= i'' && i'' < m
            , 0 <= j'' && j'' < n
            ]
       in mapM_
            (\p@(i'', j'') ->
               if board !! i'' !! j'' == ' '
                 then when (p `Set.notMember` ts) $ visit p
                 else modify $ second $ Set.insert p)
            neighbors
