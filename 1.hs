--    __________  ____  __  ______     ___
--   / ____/ __ \/ __ \/ / / / __ \   <  /
--  / / __/ /_/ / / / / / / / /_/ /   / / 
-- / /_/ / _, _/ /_/ / /_/ / ____/   / /  
-- \____/_/ |_|\____/\____/_/       /_/   
--
-- Jonathan Cheshire 1913470
-- Janhavi Singhal   [student num]
-- Sasha Tribe       [student num]

import Data.List
import Data.Maybe

--1.
--i)
average :: [Int] -> Double
average xs = fromIntegral (sum xs) / fromIntegral (length xs)

howManyBelowAverage :: Int -> Int -> Int -> Int
howManyBelowAverage a b c = length [x | x <- xs, fromIntegral x < average xs]
    where xs = [a, b, c]

--ii)
howManyBelowAverage2 :: Int -> Int -> Int -> Int
howManyBelowAverage2 a b c 
  | fromIntegral a < avg && fromIntegral b < avg = 2
  | fromIntegral a < avg && fromIntegral c < avg = 2
  | fromIntegral b < avg && fromIntegral c < avg = 2
  | fromIntegral a < avg || fromIntegral b < avg || fromIntegral c < avg = 1
  | otherwise = 0
    where avg = average [a, b, c]


q1Test :: Int -> Int -> Int -> Bool
q1Test a b c = howManyBelowAverage a b c == howManyBelowAverage2 a b c

doq1Test :: Bool
doq1Test = q1Test 1913470 1234567 7654321

--2.
truncateDecimal :: Float -> Int -> Float
truncateDecimal x n = (fromIntegral $ floor $ x * 10^n) / 10^n

pizzaPricing :: Float -> Int -> Int -> Float
pizzaPricing d t s = truncateDecimal pizzaPrice 2
    where r = d / 2 
          area = pi * r^2
          basePerSqCmPrice = 0.002
          tf = fromIntegral t
          toppingPrice = 0.001 * area
          sf = fromIntegral s
          saucePrice = 0.50
          pizzaFactor = 1.5
          pizzaPrice = pizzaFactor * ((area * basePerSqCmPrice) + (tf * toppingPrice) + (sf * saucePrice))

--3.
data Direction = North | West | South | East
  deriving (Show,Eq)

--i)
directionToInt :: Direction -> (Int, Int)
directionToInt North = (0, 1)
directionToInt South = (0, -1)
directionToInt East = (1, 0)
directionToInt West = (-1, 0)

addCoordinates :: (Int, Int) -> (Int, Int) -> (Int, Int)
addCoordinates (x1, y1) (x2, y2) = (x1+x2, y1+y2)

followDirection :: (Int, Int) -> Direction -> (Int,Int)
followDirection (x, y) direction = addCoordinates (x, y) (directionToInt direction)

testDir = (followDirection (followDirection (3,4) West) West) == (1, 4)

--ii)
followDirections :: (Int,Int) -> [Direction] -> (Int,Int)
followDirections (x, y) [] = (x, y)
followDirections (x, y) (d:ds) = followDirections (followDirection (x, y) d) ds

--iii)
data RelativeDirection = GoForward | GoBack | GoLeft | GoRight
  deriving Show

turnAround :: Direction -> Direction
turnAround North = South
turnAround South = North
turnAround West = East
turnAround East = West

turnRight :: Direction -> Direction 
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

turnLeft :: Direction -> Direction
turnLeft d = turnAround $ turnRight d

turn :: Direction -> RelativeDirection -> Direction
turn x GoForward = x
turn x GoLeft = turnLeft x
turn x GoRight = turnRight x
turn x GoBack = turnAround x

relativizeDirection :: Direction -> Direction -> RelativeDirection
relativizeDirection x1 x2 | x1 == x2  = GoForward
                          | turnAround x1 == x2 = GoBack
                          | turnLeft x1 == x2 = GoLeft
                          | otherwise = GoRight

relativizeDirections :: Direction -> [Direction] -> [RelativeDirection]
relativizeDirections x [d] = [relativizeDirection x d]
relativizeDirections x (d:ds) = rd : (relativizeDirections (turn x rd) ds)
    where rd = relativizeDirection x d

--iv)
findLastElem :: Eq a => a -> [a] -> Int
findLastElem elem xs = (length (dropWhileEnd (/=elem) xs)) - 1

mapDirections :: [Direction] -> (Int, Int) -> [(Int, Int)]
mapDirections [] coord = [coord]
mapDirections (d:ds) coord = newCoord : mapDirections ds newCoord
    where newCoord = followDirection coord d 

sanitizeHelper :: [Direction] -> [(Int, Int)] -> [Direction]
sanitizeHelper [] _ = []
sanitizeHelper ds coords = (head ds) : sanitizeHelper dsTrimmed coordsTrimmed
    where i = findLastElem (head coords) coords
          coordsTrimmed = drop (i+1) coords
          dsTrimmed = drop (i+1) ds 

sanitizeDirections :: [Direction] -> [Direction]
sanitizeDirections ds = sanitizeHelper ds coords
    where coords = mapDirections ds (0, 0)

--4.
data Orientation = H | V
  deriving (Show,Eq)
type Wall = (Int, Int, Orientation)
type Maze = ((Int,Int),[Wall])

exampleMaze :: Maze
exampleMaze = ((4,4), hWalls ++ vWalls)
    where vWalls = map (\ (i,j) -> (i,j,V)) 
                   [
                    (0,0),(0,1),(0,2),(0,3),
                          (1,1),(1,2),
                          (2,1),(2,2),
                                (3,2),(3,3),
                    (4,0),(4,1),(4,2)
                   ]
          hWalls = map (\ (i,j) -> (i,j,H))
                   [
                    (0,0),(1,0),(2,0),(3,0),
                    (0,1),      (2,1),
                                (2,2),
                    (0,4),(1,4),(2,4),(3,4)
                   ]

exampleMazeClosed :: Maze
exampleMazeClosed = ((4,4), hWalls ++ vWalls)
    where vWalls = map (\ (i,j) -> (i,j,V)) 
                   [
                    (0,0),(0,1),(0,2),(0,3),
                          (1,1),(1,2),
                          (2,1),(2,2),
                                (3,2),(3,3),
                    (4,0),(4,1),(4,2),(4,3)
                   ]
          hWalls = map (\ (i,j) -> (i,j,H))
                   [
                    (0,0),(1,0),(2,0),(3,0),
                    (0,1),      (2,1),
                                (2,2),
                    (0,4),(1,4),(2,4),(3,4)
                   ]

type Tile = ((Int, Int), (Bool, Bool, Bool, Bool))

isOutOfMaze :: Maze -> (Int, Int) -> Bool
isOutOfMaze ((w, h), _) (x, y) = x >= w || x < 0 || y >= h || y < 0 

getCoords :: Maze -> [(Int, Int)]
getCoords ((w, h), _) = [(x, y) | x <- [0..(w-1)], y <- [0..(h-1)]]

mapTile :: [Wall] -> (Int, Int) -> Tile
mapTile walls (x, y) = ((x, y), (westWall, northWall, eastWall, southWall))
    where westWall = elem (x, y, V) walls
          northWall = elem (x, y+1, H) walls
          eastWall = elem (x+1, y, V) walls
          southWall = elem (x, y, H) walls

mapTiles :: Maze -> [(Int, Int)] -> [Tile]
mapTiles ((w, h), _) [] = []
mapTiles ((w, h), walls) ((x, y):xys) = (mapTile walls (x, y)) : mapTiles ((w, h), walls) xys

getTiles :: Maze -> [Tile]
getTiles maze = mapTiles maze allCoords
    where allCoords = getCoords maze

isNoWall :: Tile -> Direction -> Bool
isNoWall (_, (w, n, e, s)) d 
  | not w && d == West = True
  | not n && d == North = True
  | not e && d == East = True
  | not s && d == South = True
  | otherwise = False

getDirectionOut :: Tile -> Direction -> Direction
getDirectionOut tile currentDir 
  | isNoWall tile left = left
  | isNoWall tile forward = forward
  | isNoWall tile right = right
  | otherwise = back 
    where left = turn currentDir GoLeft
          forward = currentDir
          right = turn currentDir GoRight
          back = turn currentDir GoBack

navigate :: Maze -> [Tile] -> (Int, Int) -> Direction -> [Maybe Direction]
navigate maze ts position facing 
  | isOutOfMaze maze position = []
  | newPosition == (0, 0) = [Nothing] 
  | otherwise = (Just nextDirection : recursiveCall)
    where tsFiltered = filter (\(xy,_) -> xy == position) ts
          currentTile = if tsFiltered /= [] then head tsFiltered else head ts
          nextDirection = getDirectionOut currentTile facing
          newPosition = followDirection position nextDirection
          recursiveCall = navigate maze ts newPosition nextDirection

getDirectionsOut :: Maze -> Maybe [Direction]
getDirectionsOut maze 
  | last result == Nothing = Nothing
  | otherwise = Just (catMaybes result)
    where tiles = getTiles maze
          result = navigate maze tiles (0, 0) North

--5.
data Btree a = Leaf a | Unary (Btree a) a | Binary (Btree a) a (Btree a)
  deriving Show

ex1 = Unary
       (Unary
         (Unary
          (Unary
           (Unary
            (Unary
             (Unary (Leaf 0) 1) 
            2) 
           3) 
          4) 
         5) 
        6) 
       7
ex2 = Binary (Binary (Leaf 0) 1 (Leaf 2)) 3 (Unary (Leaf 4) 5)
ex3 = Binary (Unary (Leaf 1) 2) 3 (Unary (Leaf 4) 5)
ex4 = Binary (Unary (Leaf 1) 2) 3 (Binary (Leaf 4) 5 (Leaf 10))

ex5 :: Btree (Int, [Char])
ex5 = Binary (Binary (Leaf (0, "a")) (1,"z") (Leaf (2,"x")))
             (3,"y")
             (Binary (Leaf (4,"b"))
                     (5,"c")
                     (Leaf (6,"d")))
exs = (ex1, ex2, ex3, ex4, ex5)

--i)
complete :: Btree a -> Bool
complete (Leaf _) = True
complete (Binary (Unary _ _) _ _) = False
complete (Binary (Binary ll _ lr) _ (Binary rl _ rr)) = 
  complete ll && complete lr && complete rl && complete rr
complete (Binary (Binary ll _ lr) _ r) = 
  complete ll && complete lr && complete r
complete (Binary l _ r) = complete l && complete r
complete (Unary (Leaf _) _) = True
complete (Unary _ _) = False 

perfect :: Btree a -> Bool
perfect (Leaf _) = True
perfect (Unary _ _) = False
perfect (Binary l _ r) = perfect l && perfect r

--ii)
lookupInSearchTree :: Int -> Btree (Int, a) -> Maybe a
lookupInSearchTree n (Leaf (k, v)) 
   | n == k = Just v
   | otherwise = Nothing
lookupInSearchTree n (Unary l (k, v)) 
   | n == k = Just v
   | n < k = lookupInSearchTree n l 
   | otherwise = Nothing
lookupInSearchTree n (Binary l (k, v) r)
   | n == k = Just v
   | n < k = lookupInSearchTree n l
   | n > k = lookupInSearchTree n r 

lookUpTest :: [Bool]
lookUpTest = (lookupInSearchTree 100 ex5 == Nothing) : map (\(k, v) -> lookupInSearchTree k ex5 == Just v) [(0, "a"), (1, "z"), (2, "x"), (3, "y"), (4, "b"), (5, "c"), (6, "d")]

--iii)
getData :: Btree (Int, a) -> (Int, a)
getData (Binary _ kv _) = kv
getData (Unary _ kv) = kv
getData (Leaf kv) = kv

insertInSearchTree :: Int -> a -> Btree (Int, a) -> Btree (Int, a)
insertInSearchTree k v (Leaf (k2, v2)) 
    | k <= k2 = Unary (Leaf (k, v)) (k2, v2)
    | otherwise = Unary (Leaf (k2, v2)) (k, v)
insertInSearchTree k v (Unary l (k2, v2))
    | k <= k2 = Unary (insertInSearchTree k v l) (k2, v2)
    | otherwise = Binary l (k2, v2) (Leaf (k, v))
insertInSearchTree k v (Binary l (k2, v2) r) 
    | k <= k2 = Binary (insertInSearchTree k v l) (k2, v2) r
    | otherwise = Binary l (k2, v2) (insertInSearchTree k v r)

