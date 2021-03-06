module Solver where

import qualified Data.Set as S
import Data.Map.Strict (Map, (!))
-- (!) - The value at the given index in an array.


paths :: Ord a => Map a [a] -> a -> a -> [[a]]
paths graph s e = dfs (S.singleton s) [s] =<< graph ! s
  where
    dfs seen (p:acc) n 						--  depth-first search
      | n == e          = [n:p:acc]			-- if current point equals end point
      | S.member n seen = []				-- if current point is`n wall
      | otherwise       = dfs (S.insert n seen) (n:p:acc) =<< graph ! n
      -- so we can`t add it to path

-- Recursive backtracker(solver): 
-- This will find a solution, but it won't
--  necessarily find the shortest solution. It focuses on you,
-- is fast for all types of Mazes, and uses stack space up to 
-- the size of the Maze. Very simple: If you're at a wall 
-- (or an area you've already plotted), return failure, else if
--  you're at the finish, return success, else recursively try
--  moving in the four directions. Plot a line when you try a 
-- new direction, and erase a line when you return failure, and 
-- a single solution will be marked out when you hit success. 
-- When backtracking, it's best to mark the space with a 
-- special visited value, so you don't visit it again from 
-- a different direction. In Computer Science terms this is
-- basically a depth first search. This method will always find 
-- a solution if one exists, but it won't necessarily be the shortest solution.
