import Data.List
import System.Environment

makeInteger :: String -> Int
makeInteger = read

main = do
   args <- getArgs
   content <- readFile (args !! 0)

   let total = (read $ (args !! 1))
   let linesOfFiles = lines content
   let ints = sort $ map makeInteger linesOfFiles

   let pair = findPairThatSumsTo ints total
   mapM_ putStrLn $ generateOutput "Pair" total pair

   putStrLn ""
   
   let triple = findTripleThatSumsTo ints total
   mapM_ putStrLn $ generateOutput "Triple" total triple
   
findPairThatSumsTo :: [Int] -> Int -> [Int]
findPairThatSumsTo xs t = findPairThatSumsToHelper xs (reverse xs) t

findPairThatSumsToHelper :: [Int] -> [Int] -> Int -> [Int]
findPairThatSumsToHelper [] _ _ = []
findPairThatSumsToHelper _ [] _ = []
findPairThatSumsToHelper up down t =
  let low = head up
      high = head down
  in if low + high == t
     then [low, high]
     else if (low + high) >= t
          then findPairThatSumsToHelper up (tail down) t
          else findPairThatSumsToHelper (tail up) down t

findTripleThatSumsTo :: [Int] -> Int -> [Int]
findTripleThatSumsTo xs t = findTripleThatSumsToHelper xs (reverse xs) t

findTripleThatSumsToHelper :: [Int] -> [Int] -> Int -> [Int]
findTripleThatSumsToHelper [] _ _ = []
findTripleThatSumsToHelper _ [] _ = []
findTripleThatSumsToHelper up down t =
  let first = head up
  in let pair = findPairThatSumsToHelper (tail up) down (t - first)
     in if null pair
        then findTripleThatSumsToHelper (tail up) down t
        else first:pair
             
generateOutput :: String -> Int -> [Int] -> [String]
generateOutput name total elts = [
  "---- " ++ name ++ " Summing To " ++ (show total) ++ " ----",
  "Elements: " ++ (show elts),
  "Total: " ++ (show $ sum elts),
  "Product: " ++ (show $ product elts)]
