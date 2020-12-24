import Data.Foldable
import Data.Maybe
import qualified Data.IntMap.Strict as Im
import Data.List.Split
import qualified Data.Sequence as S
import System.Environment

-- We represent the config using a map of next pointers.
data Config = C { arr :: Im.IntMap Int,
                  n :: Int,
                  c :: Int } deriving (Show)

main = do
  let part_one_list = map (\c -> read [c]) "712643589"
      part_one_config = C (buildMap part_one_list) (length part_one_list) (head part_one_list)
      part_one_final_config@(C m _ _) = iterate move part_one_config !! 100
  putStrLn $ "Part One: " ++ (concat $ map show (cycleStartingWith m (next m 1) 1))

  let part_two_list = (map (\c -> read [c]) "712643589" :: [Int]) ++ [10..1000000]
      part_two_config = C (buildMap part_two_list) (length part_two_list) (head part_two_list)
      part_two_final_config@(C m _ _) = iterate move part_two_config !! 10000000
      n1 = next m 1
      n2 = next m n1
  putStrLn $ "Part Two: " ++ (show (n1, n2, n1*n2))
 
buildMap :: [Int] -> Im.IntMap Int
buildMap l =
  foldr (\(c, n) m -> Im.insert c n m) Im.empty (zip l (tail l ++ [head l]))

buildConfig :: [Int] -> Config
buildConfig l =
  C (buildMap l) (length l) (head l)

next :: Im.IntMap Int -> Int -> Int
next m c = fromJust (Im.lookup c m)

grabSpanAfter :: Im.IntMap Int -> Int -> Int -> [Int]
grabSpanAfter m c num_to_take =
  take num_to_take $ tail (iterate (next m) c)

spliceOutSpan :: Im.IntMap Int -> Int -> [Int] -> Im.IntMap Int
spliceOutSpan m source span =
  let span_end = last span
      after_span = next m span_end
  in Im.insert source after_span m

spliceInSpan :: Im.IntMap Int -> [Int] -> Int -> Im.IntMap Int
spliceInSpan m span source =
  let span_end = last span
  in foldr (\(k, v) m -> Im.insert k v m) m [(source, head span), (last span, next m source)]

computeDestination :: Int -> Int -> [Int] -> Int
computeDestination current n span =
  let target = if current == 1 then n else current - 1
  in if elem target span then computeDestination target n span else target

move :: Config -> Config
move cf@(C m n c) =
  let picked_up = grabSpanAfter m c 3
      m' = spliceOutSpan m c picked_up
      destination = computeDestination c n picked_up
      m'' = spliceInSpan m' picked_up destination
  in C m'' n (next m'' c)

cycleStartingWith :: Im.IntMap Int -> Int -> Int -> [Int]
cycleStartingWith m c end =
  let follow = next m c
  in if follow == end then [c]
     else c : cycleStartingWith m follow end
         
