module Algorithm.Backtracker where

import Utils
import qualified Data.Set as S
import System.Random (StdGen, randomR)

generate :: (Int,Int) -> StdGen -> Graph
generate (n,m) g = r (start n m) (S.singleton (x,y)) [(x,y)] (x,y) g1
  where -- create field n*m | Create a singleton set with (x,y) in it
    (x, g1) = randomR (0, n-1) g -- random value for x | new StdGen using the old one
    (y, g2) = randomR (0, m-1) g1 -- random value for y | new StdGen using the old one
    r graph _    [] _ _ = graph -- if set is empty - return empty field only
    r graph seen xs c g -- else
      | null ns   = r graph seen (tail xs) (head xs) g
      | otherwise = r (connect graph nc c) (S.insert nc seen) (c:xs) nc ng
      where
        ns       = filter (`S.notMember` seen) $ neighbors (n,m) c
        (nc, ng) = sample ns g -- random value from ns with g as StdGen

-- Recursive backtracker(generator): 
-- This is somewhat related to the recursive backtracker solving method, 
-- and requires stack up to the size of the Maze. When carving, be as
-- greedy as possible, and always carve into an unmade section if one is 
-- next to the current cell. Each time you move to a new cell, push 
-- the former cell on the stack. If there are no unmade cells next to 
-- the current position, pop the stack to the previous position. 
-- The Maze is done when you pop everything off the stack.
-- This algorithm results in Mazes with about as high a "river" 
-- factor as possible, with fewer but longer dead ends, and usually 
-- a very long and twisty solution. When implemented efficiently it runs
-- fast, with only highly specialized algorithms being faster. 
-- Recursive backtracking doesn't work as a wall adder, 
-- because doing so tends to result in a solution path that follows
-- the outside edge, where the entire interior of the Maze 
-- is attached to the boundary by a single stem.
