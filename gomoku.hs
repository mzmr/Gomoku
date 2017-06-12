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

mapSize = 5

createBoard :: Board
createBoard = Board $ (replicate mapSize . replicate mapSize) Empty

printBoard :: Board -> String
printBoard (Board []) = ('\n' :) $ take mapSize ['A'..]
printBoard (Board b@(x:xs))
  = (printRow x) ++ "  " ++ show (mapSize - length b + 1) ++ "\n" ++ printBoard (Board xs)
  where
    printRow :: [Intersection] -> String
    printRow [] = []
    printRow (y:ys) = show y ++ printRow ys

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
getLineStarts i (x,y) board = foldr getStart [] [(l,d,r,u,RU), (l,y,r,y,R), (l,u,r,d,RD), (x,u,x,d,D)]
  where
    l = x - 1
    r = x + 1
    u = y - 1
    d = y + 1
    getStart :: (Int,Int,Int,Int,Direction) -> [(Coords,Direction)] -> [(Coords,Direction)]
    getStart (x1,y1,x2,y2,dir) list
      | (getISafe (x1,y1) board /= i) && (getISafe (x2,y2) board == i) = ((x,y),dir) : list
      | otherwise = list

lineLength :: (Coords,Direction) -> Board -> Int
lineLength (coords@(x,y),dir) b
  | getISafe (newX,newY) b == getIntersection coords b = (lineLength ((newX,newY),dir) b) + 1
  | otherwise = 1
  where
    newX :: Int
    newX 
      | dir == D = x
      | otherwise = x + 1

    newY :: Int
    newY
      | dir == RU = y - 1
      | dir == R = y
      | otherwise = y + 1

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
    inters = getISafe c board
    coords = [(_x,_y)| _x <- [x-1..x+1], _y <- [y-1..y+1], (_x,_y) /= (x,y)]
    isTheSame :: Coords -> Int
    isTheSame x
      | getISafe x board == inters = 1
      | otherwise = 0

points4AllCoordsAndNeighbors :: Intersection -> Board -> Int
points4AllCoordsAndNeighbors inter b = (sum $ map points4Coords coords) + (sum $ map (`points4Neighbors` b) coords)
  where
    coords = filter (\x -> getIntersection x b == inter) allCoords

points4AllLines :: Intersection -> Board -> Int
points4AllLines inter board = sum $ map points4Line allLengths
  where
    allLengths = map (`lineLength` board) (startPoints inter board)

-- funkcja wybierająca następny ruch danego gracza
chooseNext :: Intersection -> Board -> Board
chooseNext inter b
  = getNode $ (possibilities (generateMoves inter b)) !! (maybeToNormal indexOfMax)
  where
    getNode :: Tree.Tree Board -> Board
    getNode (Tree.Node node _) = node

    possibilities :: Tree.Tree Board -> [Tree.Tree Board]
    possibilities (Tree.Node _ subNodes) = subNodes

    maybeToNormal :: Maybe Int -> Int
    maybeToNormal (Just x) = x
    maybeToNormal Nothing = -1

    indexOfMax :: Maybe Int
    indexOfMax = elemIndex (maximum (maxList (generateMoves inter b))) (maxList (generateMoves inter b))
    
    maxList :: Tree.Tree Board -> [Int]
    maxList (Tree.Node _ subNodes) = map (`getMax` 2) subNodes

    getMax :: Tree.Tree Board -> Int -> Int
    getMax (Tree.Node node _) 0 = assessBoard inter node
    getMax (Tree.Node _ subNodes) level
      = sum $ map (`getMax` (level - 1)) subNodes -- maximum can be changed to sum