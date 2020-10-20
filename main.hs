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
printLineH [] s = print s
printLineH (l:ls) s =
                let currentLine = tradeInPiece s l
                in printLineH ls currentLine


main :: IO ()
main = printBoard startBoard