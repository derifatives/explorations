import System.Environment

main = do
  args <- getArgs
  content <- readFile (args !! 0)

  let ids = map boardingPassId (lines content)
      min = minimum ids
      max = maximum ids
  putStrLn $ "(Min, Max): " ++ show (min, max)
  putStrLn $ "My Seat: " ++ show ((sum [min..max]) - sum(ids))

charToBit :: Char -> Int
charToBit 'F' = 0
charToBit 'B' = 1
charToBit 'L' = 0
charToBit 'R' = 1
charToBit _ = error "Unkown char."

boardingPassId :: String -> Int
boardingPassId b = foldr (\c t -> 2 * t + (charToBit c)) 0 (reverse b)
