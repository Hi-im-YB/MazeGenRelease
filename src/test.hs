import Control.Arrow ((***))
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import qualified Data.Map.Strict as M
import Data.STRef
import qualified Data.Set as S
import System.Random
import Data.Map.Strict (Map, (!))
import System.Random (StdGen, randomR)

type Coord = (Int, Int)
type Graph = M.Map Coord [Coord]

-- build empty field of maze
start :: Int -> Int -> Graph
start n m = M.fromList [((x,y), []) | x <- [0..n-1], y <- [0..m-1]]

-- get list of neighbors for currrent point
neighbors :: Coord -> Coord -> [Coord]
neighbors (n,m) (x,y) = filter f $ map ((+) x *** (+) y) [(1,0),(0,1),(-1,0),(0,-1)] -- *** - Split the input between the two argument arrows and combine their output
  where                                                                              -- map ((+) x *** (+) y) [(1,0),(0,1),(-1,0),(0,-1)] -> [(n+1, m+0),...]
    f (x,y) = not $ x < 0 || y < 0 || x >= n || y >= m                               -- проверка на заступ за поле

-- обновление данных
connect :: Graph -> Coord -> Coord -> Graph
connect g a b = M.adjust (\x -> b:x) a $ M.adjust (\x -> a:x) b g

-- получение случайного числа в границах длины поля
sample :: [a] -> StdGen -> (a, StdGen)
sample xs g = (xs!!i, ng)
  where
    (i, ng) = randomR (0, length xs - 1) g

-- проверка, содержится ли элемент (и если нет - то добавить)
connected :: Ord a => M.Map a [a] -> [S.Set a] 
connected g = r (M.keys g) S.empty 
  where
    r []     _ = [] 
    r (x:xs) v = if x `S.member` v then r xs v else s xs v [x] S.empty 
    s xs v    []  w                  = w : r xs (v `S.union` w) 
    s xs v (y:ys) w | y `S.member` w = s xs v ys w
                    | otherwise      = s xs v (ys ++ (g M.! y)) (S.insert y w)


acyclic :: Graph -> Bool
acyclic graph = f S.empty (-1,-1) (0,0)
  where --        не содержится ли     в     | all p xs = and (map p xs)      
    f seen prev node = S.notMember node seen && all (f (S.insert node seen) node) newNodes
      where
        newNodes = filter (/=prev) (graph M.! node) -- сортировка элементов


closedWalls :: (Int,Int) -> Graph -> [(Coord, Coord)] -- defference of 2 sets
closedWalls (n,m) graph = S.toList (S.fromList all S.\\ S.fromList occupied)
  where -- [1] [[coords]] | [2] [values] | (>>=) :: (Monad m) => m a –> (a –> m b) –> m b
    all      = M.keys graph >>= \x ->  map ((,) x) (neighbors (n,m) x)
    occupied = M.toList graph >>= \(k, ks) -> map ((,) k) ks -- (,) ~ \x –> x

-- удаление стен
removeRandomWalls :: StdGen -> (Int, Int) -> Int -> Graph -> Graph
removeRandomWalls g (n,m) c graph = foldl (\g (a,b) -> connect g a b) graph chosen
  where
    options = closedWalls (n,m) graph
    chosen  = take c (fst (shuffle options g))

nonPerfect :: ((Int, Int) -> StdGen -> Graph) -> Int -> (Int,Int) -> StdGen -> Graph
nonPerfect perfect c (n,m) g = removeRandomWalls g (n,m) c (perfect (n,m) g)


-- data STRef s a
-- a value of type STRef s a is a mutable variable in state thread s, containing a value of type a
--
-- 'x <- action' runs the IO 'action', gets its result, and binds it to 'x'
shuffle :: [a] -> StdGen -> ([a],StdGen)
shuffle xs gen = runST (do
        g <- newSTRef gen -- Build a new STRef in the current state thread
        let randomRST lowhigh = do -- Promote a function to a monad.
              (a,s') <- liftM (randomR lowhigh) (readSTRef g)
              writeSTRef g s' -- Read|Write the value of an STRef -> g
              return a
        ar <- newArray n xs
        xs' <- forM [1..n] $ \i -> do
                j <- randomRST (i,n) -- random vvalue from i to n 
                vi <- readArray ar i
                vj <- readArray ar j
                writeArray ar j vi
                return vj
        gen' <- readSTRef g
        return (xs',gen'))
  where
    n = length xs
    newArray :: Int -> [a] -> ST s (STArray s Int a)
    newArray n xs =  newListArray (1,n) xs

-- ################################################################


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

-- ############################ 

-- (!) - The value at the given index in an array.


paths :: Ord a => Map a [a] -> a -> a -> [[a]]
paths graph s e = dfs (S.singleton s) [s] =<< graph ! s
  where
    dfs seen (p:acc) n            --  depth-first searchs
      | n == e          = [n:p:acc]     -- if current point equals end one
      | S.member n seen = []
      | otherwise       = dfs (S.insert n seen) (n:p:acc) =<< graph ! n