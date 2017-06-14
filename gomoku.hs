import Data.Tree as Tree
import Data.List

data Intersection
  = Black
  | White
  | Empty
  deriving (Eq)

data Board = Board [[Intersection]]

data Direction
  = RU
  | R
  | RD
  | D
  deriving (Show,Eq)

type Coords = (Int,Int)

instance Show Intersection where
  show Black = "X"
  show White = "O"
  show Empty = "-"

instance Show Board where
  show = printBoard

mapSize = 19 :: Int
searchDepth = 2 :: Int

createBoard :: Board
createBoard = Board $ (replicate mapSize . replicate mapSize) Empty

-- printBoard :: Board -> String
printBoard (Board []) = ('\n' :) $ take mapSize ['A'..]
printBoard (Board b@(x:xs))
  = (x >>= show) ++ "  " ++ show (mapSize - length b + 1) ++ "\n" ++ printBoard (Board xs)

-- ten printBoard ladniejszy, ale bez numerowanych wierszy i kolumn
-- printBoard :: Board -> String
-- printBoard (Board b) = (unlines . map (concatMap ((' ':) . show))) b

putStone :: Intersection -> Coords -> Board -> Board
putStone inters (x,y) (Board board) =
  let (a,b) = splitAt y board
      (i,j) = splitAt x (head b)
  in Board $ a ++ [i ++ [inters] ++ (tail j)] ++ (tail b)

generateMoves :: Intersection -> Board -> Tree.Tree Board
generateMoves inters board
  = Tree.Node board $ (generateMoves . oppositeColor $ inters) `map` possibleMoves
  where
    possibleMoves :: [Board]
    possibleMoves = map (\c -> putStone inters c board) (emptyCoords board)

nonEmptyCoords :: Board -> [Coords]
nonEmptyCoords b = filter (`isNotEmpty` b) allCoords

emptyCoords :: Board -> [Coords]
emptyCoords b = filter (`isEmpty` b) allCoords

allCoords :: [Coords]
allCoords = [(x,y)| x <- [0..(mapSize-1)], y <- [0..mapSize-1]]

isEmpty :: Coords -> Board -> Bool
isEmpty coords board = Empty == getIntersection coords board

isNotEmpty :: Coords -> Board -> Bool
isNotEmpty coords board = Empty /= getIntersection coords board

getIntersection :: Coords -> Board -> Intersection
getIntersection (x,y) (Board board) = (board !! y) !! x

getISafe :: Coords -> Board -> Intersection
getISafe (x,y) board
  | x < 0 || x > (mapSize-1) || y < 0 || y > (mapSize-1) = Empty
  | otherwise = getIntersection (x,y) board

oppositeColor :: Intersection -> Intersection
oppositeColor Black = White
oppositeColor White = Black
oppositeColor _ = Empty

assessBoard :: Intersection -> Board -> Int
assessBoard i b = points i - points (oppositeColor i)
  where
    points :: Intersection -> Int
    points x = points4AllCoordsAndNeighbors x b + points4AllLines x b

-- wybieram tylko te kamienie, które mają z jakiejś strony sąsiada, a z przeciwnej nie mają
startPoints :: Intersection -> Board -> [(Coords,Direction)]
startPoints inters board = getStartPoints allCoords
  where
    getStartPoints :: [Coords] -> [(Coords,Direction)]
    getStartPoints [] = []
    getStartPoints (c:xs)
      | getIntersection c board == inters = (getLineStarts inters c board) ++ (getStartPoints xs)
      | otherwise = getStartPoints xs

getLineStarts :: Intersection -> Coords -> Board -> [(Coords,Direction)]
getLineStarts i c@(x,y) board = foldr getStart [] [((l,d),(r,u),RU), ((l,y),(r,y),R), ((l,u),(r,d),RD), ((x,u),(x,d),D)]
  where
    l = x - 1
    r = x + 1
    u = y - 1
    d = y + 1
    getStart :: (Coords,Coords,Direction) -> [(Coords,Direction)] -> [(Coords,Direction)]
    getStart (c1,c2,dir) list
      | (getISafe c1 board /= i) && (getISafe c2 board == i) = (c,dir) : list
      | otherwise = list

lineLength :: (Coords,Direction) -> Board -> Int
lineLength (coords,dir) b = countLength $ newCoords coords
  where
    myInter = getIntersection coords b
    newCoords = newCoordsPattern dir

    countLength :: Coords -> Int
    countLength c
      | (getISafe c b) == myInter = (+1) $ countLength $ newCoords c
      | otherwise = 1

    newCoordsPattern :: Direction -> Coords -> Coords
    newCoordsPattern RU = \(x,y) -> (x+1,y-1)
    newCoordsPattern R = \(x,y) -> (x+1,y)
    newCoordsPattern RD = \(x,y) -> (x+1,y+1)
    newCoordsPattern D = \(x,y) -> (x,y+1)


points4Line :: Int -> Int
points4Line length
  | length < 3 = 0
  | length == 3 = 6
  | length == 4 = 10
  | length == 5 = 100000
  | otherwise = 1

points4Coords :: Coords -> Int
points4Coords (x,y) = round $ maxDistance - (distanceToCenter x y)
  where
    distanceToCenter a b = sqrt . fromIntegral $ (a-mapCenter)^2 + (b-mapCenter)^2
    maxDistance = distanceToCenter 0 0
    mapCenter = div (mapSize-1) 2

points4Neighbors :: Coords -> Board -> Int
points4Neighbors c b = 2 * countNeighbors c b

countNeighbors :: Coords -> Board -> Int
countNeighbors c@(x,y) board = sum $ map isTheSame coords
  where
    inters = getIntersection c board
    coords = [(_x,_y)| _x <- [x-1..x+1], _y <- [y-1..y+1], (_x,_y) /= (x,y)]
    isTheSame :: Coords -> Int
    isTheSame x
      | getISafe x board == inters = 1
      | otherwise = 0

points4AllCoordsAndNeighbors :: Intersection -> Board -> Int
points4AllCoordsAndNeighbors inter b = (sum $ map points4Coords coords) + (sum $ map (`points4Neighbors` b) coords)
  where
    coords = filter (\x -> getIntersection x b == inter) (nonEmptyCoords b)

points4AllLines :: Intersection -> Board -> Int
points4AllLines inter board = sum $ map points4Line allLengths
  where
    allLengths = map (`lineLength` board) (startPoints inter board)

-- funkcja wybierająca następny ruch danego gracza
-- chooseNext :: Intersection -> Board -> Board
-- chooseNext inter b
--   = getNode $ (possibilities (generateMoves inter b)) !! (maybeToNormal indexOfMax)
--   where
--     getNode :: Tree.Tree Board -> Board
--     getNode (Tree.Node node _) = node

--     possibilities :: Tree.Tree Board -> [Tree.Tree Board]
--     possibilities (Tree.Node _ subNodes) = subNodes

--     maybeToNormal :: Maybe Int -> Int
--     maybeToNormal (Just x) = x
--     maybeToNormal Nothing = -1

--     indexOfMax :: Maybe Int
--     indexOfMax = elemIndex (maximum (maxList (generateMoves inter b))) (maxList (generateMoves inter b))
    
--     maxList :: Tree.Tree Board -> [Int]
--     maxList (Tree.Node _ subNodes) = map (`getMax` searchDepth) subNodes

--     getMax :: Tree.Tree Board -> Int -> Int
--     getMax (Tree.Node node _) 0 = assessBoard inter node
--     getMax (Tree.Node _ subNodes) level
--       = sum $ map (`getMax` (level - 1)) subNodes -- maximum can be changed to sum

chooseNext :: Intersection -> Board -> Board
chooseNext inter b
  = getNode $ (possibilities moves) !! (maybeToNormal indexOfMax)
  where
    getNode :: Tree.Tree Board -> Board
    getNode (Tree.Node node _) = node

    possibilities :: Tree.Tree Board -> [Tree.Tree Board]
    possibilities (Tree.Node _ subNodes) = subNodes

    maybeToNormal :: Maybe Int -> Int
    maybeToNormal (Just x) = x
    maybeToNormal Nothing = -1

    indexOfMax :: Maybe Int
    indexOfMax = elemIndex (maximum maxSubNodes) maxSubNodes

    maxSubNodes = maxList moves
    moves = generateMoves inter b
    
    maxList :: Tree.Tree Board -> [Int]
    maxList (Tree.Node _ subNodes) = map (\x -> alfabeta x (searchDepth-1) (-999999999) 999999999) subNodes

    alfabeta :: Tree.Tree Board -> Int -> Int -> Int -> Int
    alfabeta (Tree.Node node []) _ _ _ = assessBoard inter node
    alfabeta (Tree.Node node _) 0 _ _ = assessBoard inter node
    alfabeta (Tree.Node _ subNodes) depth alpha beta
      | (searchDepth-depth) `mod` 2 == 0 = findMin subNodes beta
      | otherwise = findMax subNodes alpha
      where
        findMax :: [Tree.Tree Board] -> Int -> Int
        findMax [] a = a
        findMax (x:xs) a
          | newAlpha >= beta = beta
          | otherwise = findMax xs newAlpha
          where
            newAlpha = max (nextStep x) a

        findMin :: [Tree.Tree Board] -> Int -> Int
        findMin [] b = b
        findMin (x:xs) b
          | newBeta <= alpha = alpha
          | otherwise = findMin xs newBeta
          where
            newBeta = min (nextStep x) b

        nextStep = \x -> alfabeta x (depth-1) alpha beta

-- chooseNext :: Intersection -> Board -> Board
-- chooseNext inter board
--   = alfabeta board searchDepth -999999999 999999999 inter
--   where
--     alfabeta :: Tree.Tree Board -> Int -> Int -> Int -> Intersection -> Int
--     alfabeta (Tree.Node node []) _ _ _ i = assessBoard i node
--     alfabeta (Tree.Node node _) 0 _ _ i = assessBoard i node
--     alfabeta (Tree.Node _ subNodes) depth alpha beta i
--       | i == inter = findMax subNodes alpha
--       | otherwise = findMin subNodes beta
--       where
--         findMax :: [Tree.Tree Board] -> Int -> Int
--         findMax [] a = a
--         findMax (x:xs) a
--           | max (nextStep x) a >= beta = beta
--           | otherwise = findMax xs a

--         findMin :: [Tree.Tree Board] -> Int -> Int
--         findMin [] b = b
--         findMin (x:xs) b
--           | min (nextStep x) b <= alpha = alpha
--           | otherwise = findMin xs b

--         nextStep = \x -> alfabeta x (depth-1) alpha beta (oppositeColor i)

takeX :: String -> Int
takeX str = (\(Just x) -> x) $ elemIndex (str !! 0) ['A'..]

takeY :: String -> Int
takeY str = (1 `subtract`) $ read $ tail str

isTheEnd :: Intersection -> Board -> Bool
isTheEnd inter board = elem 5 $ map (`lineLength` board) (startPoints inter board)

play1 board = do
  putStrLn "Podaj współrzędne (np. F7):"
  coords <- getLine
  _b <- return $ putStone White (takeX coords, takeY coords) board
  putStrLn $ show _b
  if isTheEnd White _b
    then do
      putStrLn "OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO"
      putStrLn "OOO   Biały gracz wygrywa!   OOO"
      putStrLn "OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO"
    else do
      play2 _b

play2 board = do
  _b <- return $ chooseNext Black board
  putStrLn $ show _b
  if isTheEnd Black _b
    then do
      putStrLn "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
      putStrLn "XXX   Czarny gracz wygrywa!   XXX"
      putStrLn "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    else do
      play1 _b

main = do
  putStrLn "Kto zaczyna? [w|b]"
  c <- getLine
  if c == "w"
    then do
      play1 createBoard
    else do
      play2 createBoard