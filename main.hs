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

-- possibleMoves :: Board -> Pos -> [(Pos, Pos)]
-- possibleMoves board (x,y) = 
--     [((x,y), (nx,ny)) |
--     (dx, dy) <- moveDirection color,
--     let nx = x + dx,
--     let ny = y + dy,
--     inBorder board (nx,ny),
--     checkLegalMove board (x,y) (nx, ny)]
--     where 
--         color = fst (board !! y !! x)
--         moveDirection Black = [(-1, 1), (1, 1)]
--         moveDirection White = [(-1, -1) , (1, -1)]  

-- allPossibleMoves :: Board -> Color -> [(Pos, Pos)]
-- allPossibleMoves board playerColor = 
--     concatMap (possibleMoves board) playerPositions
--     where
--         boardHeight = length board 
--         boardWidth = length (head board)
--         playerPositions = [(x, y) | y <- [0..(boardHeight - 1)], x <- [0..(boardWidth - 1)], isPlayerPiece (board !! y !! x) playerColor]
--         isPlayerPiece (c, Normal) color = c == color
--         isPlayerPiece _ _ = False

-- possibleTakes :: Board -> Pos -> [(Pos, Pos)]
-- possibleTakes board (x, y) = 
--     [((x,y) , (tx,ty)) |
--     (dx, dy) <- directions,
--     let nx = x + dx,
--     let ny = y + dy,
--     (inBorder board (nx,ny)), 
--     (isOpponent (enemyCell nx ny) color),
--     let tx = nx + dx,
--     let ty = ny + dy,
--     inBorder board (tx,ty),
--     board !! ty !! tx == (No, Empty)]
--     where 
--         enemyCell nx ny = board !! ny !! nx
--         color = fst (board !! y !! x)
--         directions = [(-1, -1), (1, -1), (-1, 1), (1, 1)]
--         isOpponent (c, _) playerColor = c /= No && c /= playerColor


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