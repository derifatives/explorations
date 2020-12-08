import Data.List.Split
import Data.Set (fromList, intersection, size, unions)
import System.Environment

main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let pipeline f = show . sum . map (size . f . map fromList) . splitWhen null . lines

  putStrLn $ "Union Total: " ++ ((pipeline unions) content)
  putStrLn $ "Intersection Total: " ++ ((pipeline (foldl1 intersection)) content)
