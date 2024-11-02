import Debug.Trace (trace)

data Color = White | Black | No deriving (Eq)
data Piece = Normal | King | Empty deriving (Eq)

type Cell = (Color,Piece) 

type Board = [[Cell]]

type Pos = (Int, Int)

type Move = [Pos]

showCell :: Cell -> String
showCell (_,Empty) = "*"
showCell (White,Normal) = "w"
showCell (Black,Normal) = "b"
showCell (White,King) = "W"
showCell (Black,King) = "B"

showRow :: Int -> [Cell] -> String
showRow rowIdx row = show rowIdx ++ " " ++ unwords (map showCell row)

showBoard :: Board -> String
showBoard board = foldr (\row acc -> row ++ "\n" ++ acc)  "" ( colHeader : (zipWith showRow [0..] board))
    where 
        colHeader = "  " ++ unwords (map show [0 .. length (head board) - 1])

printBoard :: Board -> IO()
printBoard board = putStrLn (showBoard board)

changeRow :: Int -> Cell -> [Cell] -> [Cell]
changeRow pos c (x:xs)
    | pos == 0 = c : xs
    | otherwise = x : changeRow (pos-1) c xs

changeBoard :: Int -> Int -> Cell -> Board -> Board
changeBoard 0 dx c (x:xs)  = changeRow dx c x : xs
changeBoard dy dx c (x:xs) = x : changeBoard (dy - 1) dx c xs

inBorder :: Board -> Pos -> Bool
inBorder board (x,y) = x >= 0 && y >= 0 && x < boardWidth && y < boardHeight
    where
        boardHeight = length board 
        boardWidth = length (head board)

checkLegalMove :: Board -> Pos -> Pos -> Bool
checkLegalMove board (startX, startY) (x,y) 
    | not (inBorder board (x,y)) = False
    | endCell /= (No,Empty) = False
    | startCell == (No,Empty) = False
    | otherwise = case startCell of 
                    (_, Normal) -> checkLegalNormalPieceMove 
                    (_, King) -> False
    where 
        endCell = board !! y !! x
        startCell = board !! startY !! startX
        checkLegalNormalPieceMove = abs (x - startX) == 1 && abs (y - startY) == 1

moves :: Board -> Pos -> [Move]
moves board (x, y) = [ [(x, y), (nx, ny)] | (dx, dy) <- direction c , let nx = x + dx , let ny = y + dy , checkLegalMove board (x, y) (nx, ny)]
    where 
        c = fst (board !! y !! x)
        direction Black = [(-1, 1), (1, 1)]
        direction White = [(-1, -1) , (1, -1)]

allMoves :: Board -> Color -> [Move]
allMoves board c = concatMap (moves board) playerPos
    where
        playerPos = [(x, y) | y <- [0..7] , x <- [0..7] , isPlayer (board !! y !! x) c]
        isPlayer (c, Normal) color = c == color
        isPlayer _ _ = False

takes :: Board -> Pos -> [Move]
takes board (x, y) = 
    [[(x, y) , (tx, ty)] | (dx, dy) <- directions,
    let ex = x + dx,
    let ey = y + dy,
    inBorder board (ex, ey),
    isOpponent (enemyCell ex ey) (fst (board !! y !! x)),
    let tx = ex + dx,
    let ty = ey + dy,
    inBorder board (tx,ty),
    board !! ty !! tx == (No, Empty)]
    where 
        directions = [(-1, -1), (1, -1), (-1, 1), (1, 1)]
        enemyCell nx ny = board !! ny !! nx
        isOpponent (c, _) playerColor = c /= No && c /= playerColor

addMove :: Pos -> Move -> Move
addMove pos move = move ++ [pos] 

getLastMove :: [Move] -> [Pos]
getLastMove moves = map last moves

executeTake :: Board -> Pos -> Pos -> Board
executeTake board (sx, sy) (ex, ey) = changeBoard sy sx (No, Empty) $ changeBoard ey ex (startColor, startPiece) $ changeBoard (sy + dy) (sx + dx) (No, Empty) board
    where
        (startColor, startPiece) = board !! sy !! sx
        dx = (ex - sx) `div` 2
        dy = (ey - sy) `div` 2


findChains :: Board -> Pos -> Move -> [Move]
findChains board pos currentC = 
    case takes board pos of
        [] -> if length currentC == 1 then [] else [currentC]
        moves -> concatMap (\nextPos -> findChains (executeTake board pos nextPos) nextPos (addMove nextPos currentC)) (getLastMove moves) 

possiblePlays :: Board -> Color -> [Move]
possiblePlays board color = if null allTakes then allMoves board color else allTakes
    where
        allTakes = concatMap (\pos -> findChains board pos [pos]) playerPos
        playerPos = [(x, y) | y <- [0..7], x <- [0..7], isPlayer (board !! y !! x) color]
        isPlayer (c, Normal) col = c == col
        isPlayer (c, King) col = c == col
        isPlayer _ _ = False

initBoard :: Board
initBoard = [
           [(No, Empty)    , (Black, Normal), (No, Empty), (Black, Normal), (No, Empty), (Black, Normal), (No, Empty), (Black, Normal)],
           [(Black, Normal), (No, Empty), (Black, Normal), (No, Empty), (Black, Normal), (No, Empty), (Black, Normal), (No, Empty)],
           [(No, Empty)    , (Black, Normal), (No, Empty), (Black, Normal), (No, Empty), (Black, Normal), (No, Empty), (Black, Normal)],
           [(No, Empty)    , (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty)],
           [(No, Empty)    , (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty)],
           [(White, Normal), (No, Empty), (White, Normal), (No, Empty), (White, Normal), (No, Empty), (White, Normal), (No, Empty)],
           [(No, Empty)    , (White, Normal), (No, Empty), (White, Normal), (No, Empty), (White, Normal), (No, Empty), (White, Normal)],
           [(White, Normal), (No, Empty), (White, Normal), (No, Empty), (White, Normal), (No, Empty), (White, Normal), (No, Empty)]
        ]
testBoard :: Board
testBoard = [
           [(Black, Normal), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty)],
           [(No, Empty)    , (White, Normal), (No, Empty), (White, Normal), (No, Empty), (No, Empty), (No, Empty), (No, Empty)],
           [(No, Empty)    , (No, Empty), (No, Empty), (No, Empty), (Black, Normal), (No, Empty), (No, Empty), (No, Empty)],
           [(No, Empty)    , (No, Empty), (No, Empty), (White, Normal), (No, Empty), (No, Empty), (No, Empty), (No, Empty)],
           [(No, Empty)    , (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty)],
           [(No, Empty)    , (No, Empty), (No, Empty), (No, Empty), (No, Empty), (White, Normal), (No, Empty), (No, Empty)],
           [(No, Empty)    , (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty)],
           [(No, Empty)    , (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty), (No, Empty)]
        ]

main = do 
    printBoard testBoard
    print (possiblePlays testBoard Black)