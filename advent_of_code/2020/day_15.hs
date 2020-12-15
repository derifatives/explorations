import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M
import System.Environment

main = do
  let input = [15, 12, 0, 14, 3, 1]
      n = 30000000  -- Part One: 2020
      l = length input
      init_state = (M.fromList (zip (init input) [1..]), last input, l)
      result@(_, final, nf) = (iterate updateState init_state) !! (n - l)
  putStrLn $ (show (nf, final))

updateState :: (M.Map Integer Integer, Integer, Int) -> (M.Map Integer Integer, Integer, Int)
updateState state@(m, current, n) =
  let maybe_next = M.lookup current m
      n_ig = toInteger n
      next = if isJust maybe_next then n_ig - (fromJust maybe_next) else 0
  in (M.insert current n_ig m, next, n+1)
