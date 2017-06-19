import Data.Tree
import Data.List
import Data.Char

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

printBoard :: Board -> String
printBoard (Board []) = ('\n' :) $ take mapSize ['A'..]
printBoard (Board b@(x:xs))
  = (x >>= show) ++ "  " ++ show (mapSize - length b + 1) ++ "\n" ++ printBoard (Board xs)

-- ten printBoard ladniejszy, ale bez numerowanych wierszy i kolumn
-- printBoard :: Board -> String
-- printBoard = unlines . (map (concatMap ((' ':) . show))) . (\(Board b) -> b)

putStone :: Intersection -> Coords -> Board -> Board
putStone inters (x,y) (Board board) =
  let (a,b) = splitAt y board
      (i,j) = splitAt x (head b)
  in Board $ a ++ [i ++ [inters] ++ (tail j)] ++ (tail b)

generateMoves :: Intersection -> Board -> Tree Board
generateMoves inters board
  = Node board $ (generateMoves (oppositeColor inters)) `map` possibleMoves
  where
    possibleMoves :: [Board]
    possibleMoves = map (\c -> putStone inters c board) (emptyCoords board)

nonEmptyCoords :: Board -> [Coords]
nonEmptyCoords b = filter (not . (`isEmpty` b)) allCoords

emptyCoords :: Board -> [Coords]
emptyCoords b = filter (`isEmpty` b) allCoords

allCoords :: [Coords]
allCoords = [(x,y)| x <- [0..(mapSize-1)], y <- [0..mapSize-1]]

isEmpty :: Coords -> Board -> Bool
isEmpty coords board = Empty == getIntersection coords board

getIntersection :: Coords -> Board -> Intersection
getIntersection (x,y) (Board board) = (board !! y) !! x

getISafe :: Coords -> Board -> Intersection
getISafe c@(x,y) board
  | x < 0 || x > (mapSize-1) || y < 0 || y > (mapSize-1) = Empty
  | otherwise = getIntersection c board

oppositeColor :: Intersection -> Intersection
oppositeColor Black = White
oppositeColor White = Black
oppositeColor _ = Empty

assessBoard :: Intersection -> Board -> Int
assessBoard i b = points i - points (oppositeColor i)
  where
    points :: Intersection -> Int
    points inter = points4AllCoordsAndNeighbors inter b + points4AllLines inter b

-- wybieram tylko te kamienie, które mają z jakiejś strony sąsiada, a z przeciwnej nie mają
startPoints :: Intersection -> Board -> [(Coords,Direction)]
startPoints i b = foldr getStarts [] filtered
  where
    filtered = filter (\c -> getIntersection c b == i) allCoords
    getStarts = (++) . (\x -> getLineStarts i x b)

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
lineLength (coords,dir) b = lineLen (nextCoords dir) coords
  where
    myInter = getIntersection coords b

    myColorCoords :: [Coords]
    myColorCoords = filter ((== myInter) . (`getIntersection` b)) allCoords

    lineLen :: (Coords -> Coords) -> Coords -> Int
    lineLen newC c 
      | elem c myColorCoords = 1 + lineLen newC (newC c)
      | otherwise = 0

nextCoords :: Direction -> Coords -> Coords
nextCoords RU = \(x,y) -> (x+1,y-1)
nextCoords R = \(x,y) -> (x+1,y)
nextCoords RD = \(x,y) -> (x+1,y+1)
nextCoords D = \(x,y) -> (x,y+1)


points4Line :: Int -> Int
points4Line length
  | length < 3 = 0
  | length == 3 = 50
  | length == 4 = 100
  | length == 5 = 100000
  | otherwise = -100000

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

chooseNext :: Intersection -> Board -> Board
chooseNext inter b
  = (\(Node n _) -> n) $ moves !! indexOfMax
  where
    indexOfMax :: Int
    indexOfMax = (\(Just i) -> i) $ elemIndex (maximum maxList) maxList
    
    maxList :: [Int]
    maxList = map (\x -> alfabeta x (searchDepth-1) (-999999999) 999999999) moves

    moves :: [Tree Board]
    moves = (\(Node _ s) -> s) $ generateMoves inter b

    alfabeta :: Tree Board -> Int -> Int -> Int -> Int
    alfabeta (Node node []) _ _ _ = assessBoard inter node
    alfabeta (Node node _) 0 _ _ = assessBoard inter node
    alfabeta (Node _ subNodes) depth alpha beta
      | (searchDepth-depth) `mod` 2 /= 0 = findMin subNodes beta
      | otherwise = findMax subNodes alpha
      where
        findMax :: [Tree Board] -> Int -> Int
        findMax [] a = a
        findMax (x:xs) a
          | newAlpha >= beta = beta
          | otherwise = findMax xs newAlpha
          where
            newAlpha = max (alfabeta x (depth-1) a beta) a

        findMin :: [Tree Board] -> Int -> Int
        findMin [] b = b
        findMin (x:xs) b
          | newBeta <= alpha = alpha
          | otherwise = findMin xs newBeta
          where
            newBeta = min (alfabeta x (depth-1) alpha b) b

takeX :: String -> Int
takeX str = toInt $ elemIndex (toUpper $ str !! 0) ['A'..]
  where
    toInt :: Maybe Int -> Int
    toInt (Just x) = x
    toInt Nothing = -1

takeY :: String -> Int
takeY str = (1 `subtract`) $ read $ tail str

isTheEnd :: Intersection -> Board -> Bool
isTheEnd inter board = elem 5 $ map (`lineLength` board) (startPoints inter board)

play1 board = do
  putStrLn "Podaj współrzędne (np. F7):"
  coords <- getLine
  x <- return $ takeX coords
  y <- return $ takeY coords
  if False == (elem (x,y) allCoords)
    then do
      putStrLn "Niepoprawne współrzędne!"
      play1 board
    else do
      checkIntersection (x,y) board

checkIntersection coords board = do
  if getIntersection coords board /= Empty
    then do
      putStrLn "To pole jest już zajęte. Wybierz inne."
      play1 board
    else do
      insertNewStone coords board

insertNewStone coords board = do
  _b <- return $ putStone White coords board
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