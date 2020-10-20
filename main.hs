data Piece = Black | White | Blank
        deriving(Show, Eq)

startBoard :: [[Piece]]
startBoard = [[Blank,Blank,Blank],
              [Blank,Blank,Blank],
              [Blank,Blank,Blank],
              [Blank,Blank,Blank,Blank,Blank,Blank],
              [Blank,Blank,Blank],
              [Blank,Blank,Blank],
              [Blank,Blank,Blank]]

printBoard :: [[Piece]] -> IO ()
printBoard board = printBoardH board 0


printBoardH :: (Eq t, Num t) => [[Piece]] -> t -> IO ()
printBoardH [] _ = putStrLn ""
printBoardH (line:board) n = do
                                printLine line n
                                printBoardH board (n+1)

getDefaultLine :: (Eq a, Num a) => a -> [Char]
getDefaultLine n = case n of 
                      0 -> "_-----------_-----------_\n|           |           |"
                      1 -> "|   _-------_-------_   |\n|   |       |       |   |"
                      2 -> "|   |   _---_---_   |   |\n|   |   |       |   |   |"
                      3 -> "_---_---_       _---_---_\n|   |   |       |   |   |"
                      4 -> "|   |   _---_---_   |   |\n|   |       |       |   |"
                      5 -> "|   _-------_-------_   |\n|           |           |"
                      6 -> "_-----------_-----------_"

tradeInPiece :: [Char] -> Piece -> [Char]
tradeInPiece [] _ = error "wasn't able to trade in a piece because there was no empty room"
tradeInPiece (x:xs) p | x == '_' = pieceToChar p : xs
                      | otherwise = x : tradeInPiece xs p

pieceToChar :: Piece -> Char
pieceToChar Black = 'B'
pieceToChar White = 'W'
pieceToChar Blank = 'o'

printLine :: (Eq a, Num a) => [Piece] -> a -> IO ()
printLine l n = printLineH l (getDefaultLine n)

printLineH :: [Piece] -> [Char] -> IO ()
printLineH [] s = putStrLn s
printLineH (l:ls) s =
                let currentLine = tradeInPiece s l
                in printLineH ls currentLine

placePiece :: [[Piece]] -> Piece -> (Int, Int) -> [[Piece]]
placePiece [] _ _ = error "placing the new piece wasn't possible because the row wasn't valid"
placePiece (line:board) p (y,x) | y == 0 = (placePieceInLine line p x) : board
                                | y > 0 = line : placePiece board p ((y-1),x)

placePieceInLine :: [Piece] -> Piece -> Int -> [Piece]
placePieceInLine [] _ _ = error "placing the new piece wasn't possible because the column wasn't valid"
placePieceInLine (l:ls) p x | x == 0 && l == Blank = p : ls
                            | x > 0 = l : placePieceInLine ls p (x-1)
                            | l /= Blank = error "coudln't place piece because field wasn't empty"

main :: IO ()
main = do
        let b = startBoard
        printBoard b
        printBoard $ placePiece (placePiece b Black (3,3)) White (3,2)