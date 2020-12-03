import Data.List
import System.Environment

makeInteger :: String -> Int
makeInteger = read

main = do
   args <- getArgs
   content <- readFile (args !! 0)
   let ints = sort $ map makeInteger $ lines content
       total = read $ (args !! 1)
       pair = findPairThatSumsTo ints total
       triple = findTripleThatSumsTo ints total
       
   mapM_ putStrLn $ generateOutput "Pair" total pair
   putStrLn ""
   mapM_ putStrLn $ generateOutput "Triple" total triple
   
findPairThatSumsTo :: [Int] -> Int -> [Int]
findPairThatSumsTo xs t = findPairThatSumsToHelper xs (reverse xs) t

findPairThatSumsToHelper :: [Int] -> [Int] -> Int -> [Int]
findPairThatSumsToHelper [] _ _ = []
findPairThatSumsToHelper _ [] _ = []
findPairThatSumsToHelper up@(low:up_rest) down@(high:down_rest) t =
  if low + high == t
  then [low, high]
  else if (low + high) >= t
       then findPairThatSumsToHelper up down_rest t
       else findPairThatSumsToHelper up_rest down t

findTripleThatSumsTo :: [Int] -> Int -> [Int]
findTripleThatSumsTo xs t = findTripleThatSumsToHelper xs (reverse xs) t

findTripleThatSumsToHelper :: [Int] -> [Int] -> Int -> [Int]
findTripleThatSumsToHelper [] _ _ = []
findTripleThatSumsToHelper _ [] _ = []
findTripleThatSumsToHelper up@(low:up_rest) down t =
  let pair = findPairThatSumsToHelper up_rest down (t - low)
  in if null pair
     then findTripleThatSumsToHelper up_rest down t
     else low:pair
             
generateOutput :: String -> Int -> [Int] -> [String]
generateOutput name total elts = [
  "---- " ++ name ++ " Summing To " ++ (show total) ++ " ----",
  "Elements: " ++ (show elts),
  "Total: " ++ (show $ sum elts),
  "Product: " ++ (show $ product elts)]
