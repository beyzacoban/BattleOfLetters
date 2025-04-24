printBoard :: [[String]] -> IO ()
printBoard = mapM_ (putStrLn . unwords)








main :: IO ()
main = do
  let board = [["X", "A", "-", "-", "X"],
               ["B", "-", "-", "-", "Z"],
               ["X", "C", "-", "-", "X"]]
  let letters = [1, 5, 11, 9]
  let forbiddenIndexes = [0, 4, 10, 14]
  putStrLn "Welcome!"








  printBoard board
  putStrLn "Enter the maximum number of total moves allowed: "
  input <- getLine
  let max_move = read input :: Int
  putStrLn ("max: " ++ show max_move)
  askPlayer letters board forbiddenIndexes 0 max_move








askPlayer :: [Int] -> [[String]] -> [Int] -> Int -> Int -> IO ()
askPlayer letters board forbiddenIndexes moveCount maxMove = do
  putStrLn "Who starts first? Type 'last' or 'firsts': "
  first_side <- getLine
  if first_side == "firsts"
    then playFirst letters board forbiddenIndexes moveCount maxMove
    else if first_side == "last"
      then playLast letters board forbiddenIndexes moveCount maxMove
      
      else do
        putStrLn "Invalid input. Try again."
        askPlayer letters board forbiddenIndexes moveCount maxMove








playFirst :: [Int] -> [[String]] -> [Int] -> Int -> Int -> IO ()
playFirst letters board forbiddenIndexes moveCount maxMove
  | moveCount >= maxMove = putStrLn "Draw! Maximum number of moves reached."
  | otherwise = do
      putStrLn "Please select one of the first three letters and a cell to move it (e.g., A 6):"
      input <- getLine
      let (letter, cellStr) = break (== ' ') input
      let cell = read (drop 1 cellStr) :: Int
      let index = case letter of
                    "A" -> 0; "B" -> 1; "C" -> 2; _ -> -1
      if index == -1 || cell < 0 || cell > 14 then do
        putStrLn "Invalid input!"
        playLast letters board forbiddenIndexes moveCount maxMove
      else do
        let (newBoard, newCellPos) = move letter (letters !! index) cell board forbiddenIndexes
        if newBoard == board then do
          putStrLn "Invalid move!"
          playLast letters board forbiddenIndexes moveCount maxMove
        else do
          let updatedLetters = take index letters ++ [newCellPos] ++ drop (index + 1) letters
          printBoard newBoard
          playLast updatedLetters newBoard forbiddenIndexes (moveCount + 1) maxMove








playLast :: [Int] -> [[String]] -> [Int] -> Int -> Int -> IO ()
playLast letters board forbiddenIndexes moveCount maxMove
  | moveCount >= maxMove = putStrLn "Draw! Maximum number of moves reached."
  | otherwise = do
      putStrLn "Please select a cell for the Z:"
      cellStr <- getLine
      let cell = read cellStr :: Int
      if cell < 0 || cell > 14 then do
        putStrLn "Invalid cell!"
        playFirst letters board forbiddenIndexes moveCount maxMove
      else do
        let (newBoard, newZPos) = move "Z" (letters !! 3) cell board forbiddenIndexes
        if newBoard == board then do
          putStrLn "Invalid move!"
          playFirst letters board forbiddenIndexes moveCount maxMove
        else if zWins newZPos letters then
          putStrLn "Z wins!"
        else do
          let updatedLetters = take 3 letters ++ [newZPos]
          printBoard newBoard
          playFirst updatedLetters newBoard forbiddenIndexes (moveCount + 1) maxMove








move :: String -> Int -> Int -> [[String]] -> [Int] -> ([[String]], Int)
move letter cell newcell board forbiddenIndexes
  | newcell `elem` forbiddenIndexes = (board, cell)
  | target /= "-" = (board, cell)
  | not (isValidMove letter cell newcell) = (board, cell)
  | otherwise = (newBoard, newcell)
  where
    row = newcell `div` 5
    col = newcell `mod` 5
    target = (board !! row) !! col
    tempBoard = updateRow newcell letter board
    newBoard = updateRow cell "-" tempBoard








isValidMove :: String -> Int -> Int -> Bool
isValidMove letter cell newcell =
  let cellRow = cell `div` 5
      cellCol = cell `mod` 5
      newRow = newcell `div` 5
      newCol = newcell `mod` 5
      rowOk = abs (cellRow - newRow) <= 1
      colOk = abs (cellCol - newCol) <= 1
      notLeft = newCol >= cellCol
   in rowOk && colOk && (letter == "Z" || notLeft)






updateRow :: Int -> String -> [[String]] -> [[String]]
updateRow idx val board =
  let row = idx `div` 5
      col = idx `mod` 5
      currentRow = board !! row
      updatedRow = take col currentRow ++ [val] ++ drop (col + 1) currentRow
  in take row board ++ [updatedRow] ++ drop (row + 1) board








zWins :: Int -> [Int] -> Bool
zWins zPos firsts = all (\pos -> (zPos `mod` 5) < (pos `mod` 5)) (take 3 firsts)











































