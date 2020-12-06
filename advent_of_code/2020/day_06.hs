import Data.List.Split
import Data.List.Unique
import qualified Data.Set as Set
import System.Environment

main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let groups = splitWhen null (lines content)
  -- let per_group = map sortUniq groups
  -- let total = sum $ map length per_group

  let union_total =
        (sum $ map (length . sortUniq . concat) groups)
  let intersection_total =
        (sum $ map (
            length . (
                foldr (\l c -> (Set.fromList l) `Set.intersection` c) (Set.fromList ['a'..'z']))) groups)


  putStrLn $ "Union Total: " ++ (show union_total)
  putStrLn $ "Intersection Total: " ++ (show intersection_total) --
