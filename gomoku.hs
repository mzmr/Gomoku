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
createBoard = Board $ replicate mapSize $ replicate mapSize Empty

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
  = Tree.Node board $ (generateMoves . oppositeColor $ inters) `map` possibleMoves allCoords
  where
    possibleMoves :: [Coords] -> [Board]
    possibleMoves [] = []
    possibleMoves (c:xs)
      | isEmpty c board = (putStone inters c board) : (possibleMoves xs)
      | otherwise = possibleMoves xs

allCoords :: [Coords]
allCoords = [(x,y)| x <- [0..(mapSize-1)], y <- [0..mapSize-1]]

isEmpty :: Coords -> Board -> Bool
isEmpty coords board = Empty == getIntersection coords board

getIntersection :: Coords -> Board -> Intersection
getIntersection (x,y) (Board board) = (board !! y) !! x

getISafe :: Coords -> Board -> Intersection
getISafe (x,y) board
  | x < 0 || x > (mapSize-1) || y < 0 || y > (mapSize-1) = Empty
  | otherwise = getIntersection (x,y) board

oppositeColor :: Intersection -> Intersection
oppositeColor inters
  | inters == Black = White
  | inters == White = Black
  | otherwise = Empty

assessBoard :: Intersection -> Board -> Int
assessBoard i b = points i - points (oppositeColor i)
  where
    points :: Intersection -> Int
    points x = points4AllCoordsAndNeighbors x b + points4AllLines x b

-- wybieram tylko te kamienie, które mają z jakiejś strony sąsiada, a z przeciwnej nie mają
startPoints :: Intersection -> Board -> [(Coords,Direction)]
startPoints inters board = getStartPoints inters allCoords board
  where
    getStartPoints :: Intersection -> [Coords] -> Board -> [(Coords,Direction)]
    getStartPoints _ [] _ = []
    getStartPoints inters (c:xs) board
      | getIntersection c board == inters = (getLineStarts inters c board) ++ (getStartPoints inters xs board)
      | otherwise = getStartPoints inters xs board

getLineStarts :: Intersection -> Coords -> Board -> [(Coords,Direction)]
getLineStarts i (x,y) board = getStart [l,d,r,u] RU $ getStart [l,y,r,y] R $ getStart [l,u,r,d] RD $ getStart [x,u,x,d] D []
  where
    l = x - 1
    r = x + 1
    u = y - 1
    d = y + 1
    getStart :: [Int] -> Direction -> [(Coords,Direction)] -> [(Coords,Direction)]
    getStart [x1,y1,x2,y2] dir list
      | (getISafe (x1,y1) board /= i) && (getISafe (x2,y2) board == i) = ((x,y),dir) : list
      | otherwise = list

nextX :: Int -> Direction -> Int
nextX x dir
  | dir /= D = x + 1
  | otherwise = x

nextY :: Int -> Direction -> Int
nextY y dir
  | dir == RU = y - 1
  | dir == R = y
  | otherwise = y + 1

lineLength :: (Coords,Direction) -> Board -> Int
lineLength (coords@(x,y),dir) b
  | getISafe (newX,newY) b == getIntersection coords b = (lineLength ((newX,newY),dir) b) + 1
  | otherwise = 1
  where
    newX = nextX x dir
    newY = nextY y dir

points4Line :: Int -> Int
points4Line length
  | length < 3 = 0
  | length == 3 = 6
  | length == 4 = 10
  | length == 5 = 100000
  | otherwise = 1

points4Coords :: Coords -> Int
points4Coords (x,y) = round $ maxDistance - (distanceToCenter x y) -- 13, bo distanceToCenter moze najwiecej wyjsc ~12.73, więc wychodzi zakres punktów ~[0; 13]
  where
    distanceToCenter a b = sqrt $ fromIntegral $ (a-mapCenter)^2 + (b-mapCenter)^2
    maxDistance = distanceToCenter 0 0
    mapCenter = div (mapSize-1) 2

points4Neighbors :: Coords -> Board -> Int
points4Neighbors c b = 2 * countNeighbors c b

countNeighbors :: Coords -> Board -> Int
countNeighbors c@(x,y) board = countIt coords
  where
    inters = getISafe c board
    coords = [(x-1,y-1),(x,y-1),(x+1,y-1),(x-1,y),(x+1,y),(x-1,y+1),(x,y+1),(x+1,y+1)]
    countIt :: [Coords] -> Int
    countIt [] = 0
    countIt (x:xs)
      | getISafe x board == inters = 1 + countIt xs
      | otherwise = countIt xs

points4AllCoordsAndNeighbors :: Intersection -> Board -> Int
points4AllCoordsAndNeighbors inter b = points4AllCoords coords + points4AllNeighbors coords b
  where
    coords = filterCoords allCoords
    filterCoords :: [Coords] -> [Coords]
    filterCoords [] = []
    filterCoords (x:xs)
      | getISafe x b == inter = x : filterCoords xs
      | otherwise = filterCoords xs

points4AllCoords :: [Coords] -> Int
points4AllCoords [] = 0
points4AllCoords (x:xs) = points4Coords x + points4AllCoords xs

points4AllNeighbors :: [Coords] -> Board -> Int
points4AllNeighbors [] _ = 0
points4AllNeighbors (x:xs) b = points4Neighbors x b + points4AllNeighbors xs b

points4AllLines :: Intersection -> Board -> Int
points4AllLines inter board = sum pointsForEachLength
  where
    allLengths = map (`lineLength` board) (startPoints inter board)
    pointsForEachLength = map points4Line allLengths

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
      = maximum $ map (`getMax` (level - 1)) subNodes -- można zamienić sum na maximum (zależy czy szukam jednego najlepszego node'a czy grupy node'ow dających wieksze szanse na lepsza dalsza gre)