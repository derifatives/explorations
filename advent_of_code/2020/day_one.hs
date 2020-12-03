import Data.List
import System.Environment

makeInteger :: String -> Int
makeInteger = read

main = do
   args <- getArgs
   content <- readFile (args !! 0)
   let ints = sort $ map makeInteger $ lines content
       total = read $ (args !! 1)
       pairs = pairsWithSum ints total
       triples = triplesWithSum ints total

   putStrLn ""
   putStrLn $ "All Pairs: " ++ (show pairs)
   putStrLn $ "All Triples: " ++ (show triples)
   
   mapM_ putStrLn $ generateOutput "Pair" total (head pairs)
   putStrLn ""
   mapM_ putStrLn $ generateOutput "Triple" total (head triples)
   
pairsWithSum :: [Int] -> Int -> [[Int]]
pairsWithSum xs t = pairsWithSumHelper xs (reverse xs) t

pairsWithSumHelper :: [Int] -> [Int] -> Int -> [[Int]]
pairsWithSumHelper [] _ _ = []
pairsWithSumHelper _ [] _ = []
pairsWithSumHelper up@(low:up_rest) down@(high:down_rest) t =
  if low + high == t
  then [low, high]:pairsWithSumHelper up_rest down_rest t
  else if (low + high) >= t
       then pairsWithSumHelper up down_rest t
       else pairsWithSumHelper up_rest down t

triplesWithSum :: [Int] -> Int -> [[Int]]
triplesWithSum xs t = triplesWithSumHelper xs (reverse xs) t

triplesWithSumHelper :: [Int] -> [Int] -> Int -> [[Int]]
triplesWithSumHelper [] _ _ = []
triplesWithSumHelper _ [] _ = []
triplesWithSumHelper up@(low:up_rest) down t =
  let pairs = pairsWithSumHelper up_rest down (t - low)
  in (map (\p -> low:p) pairs) ++ triplesWithSumHelper up_rest down t
     
generateOutput :: String -> Int -> [Int] -> [String]
generateOutput name total elts = [
  "---- " ++ name ++ " Summing To " ++ (show total) ++ " ----",
  "Elements: " ++ (show elts),
  "Total: " ++ (show $ sum elts),
  "Product: " ++ (show $ product elts)]
