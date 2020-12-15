import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M
import System.Environment

main = do
  let input = [15, 12, 0, 14, 3, 1]
      n = 2020  -- Part One: 2020, Part 2: 30000000
      l = length input
      init_state = (M.fromList (zip (init input) [1..]), last input, l)
      result@(_, final, nf) = (iterate updateState init_state) !! (n - l)
  putStrLn $ (show (nf, final))

updateState :: (M.Map Int Int, Int, Int) -> (M.Map Int Int, Int, Int)
updateState state@(m, current, n) =
  let next = case M.lookup current m of 
        Just last -> n - last
        Nothing -> 0
  in (M.insert current n m, next, n+1)
