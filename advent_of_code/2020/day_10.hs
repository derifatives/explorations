import Data.List
import qualified Data.Map.Strict as M
import System.Environment

main = do
  args <- getArgs
  content <- readFile (args !! 0)

  let ns' = sort (map read (lines content)) :: [Int]
      ns = 0:(ns' ++ [((last ns') + 3)])
      diffs = zipWith (-) (tail ns) ns
      count = \n -> length (filter (== n) diffs)
  putStrLn $ "Product: " ++ (show ((count 1) * (count 3)))

  let pathcounts = numPaths (tail ns) (M.singleton 0 1)
  putStrLn $ "Paths: " ++ (show (M.lookup (last ns) pathcounts))

type PathCounts = M.Map Int Int

numPaths :: [Int] -> PathCounts -> PathCounts
numPaths [] r = r
numPaths ns@(node:rest) pc =
  let v = sum $ map (\i -> M.findWithDefault 0 (node - i) pc) [1..3]
      in numPaths rest (M.insert node v pc)
