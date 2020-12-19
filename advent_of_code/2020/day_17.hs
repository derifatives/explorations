import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Environment


main = do
  args <- getArgs
  content <- readFile (args !! 0)

  let init_state_3 = parseInput (lines content) 1
  putStrLn $ "3 Dims after 6 updates: " ++ (show (S.size ((iterate (update offsets_3) init_state_3) !! 6)))

  let init_state_4 = parseInput (lines content) 2
  putStrLn $ "4 Dims after 6 updates: " ++ (show (S.size ((iterate (update offsets_4) init_state_4) !! 6)))

newtype Pos = Pos [Int] deriving (Eq, Ord, Show)

addPos :: Pos -> Pos -> Pos
addPos pos1@(Pos posl1) pos2@(Pos posl2) = Pos (map (uncurry (+)) (zip posl1 posl2))

parseInput :: [String] -> Int -> S.Set Pos
parseInput input extra_zeros =
  -- (a -> b -> b) -> b -> [a] -> b
  foldr (\l s -> processLine l extra_zeros s) S.empty (zip input [0..])

processLine :: (String, Int) -> Int -> S.Set Pos -> S.Set Pos
processLine (line, row) extra_zeros set =
  foldr processChar set (zip line [0..])
  where processChar (char, col) set =
          if char == '#'
          then S.insert (Pos ([row, col] ++ (take extra_zeros (repeat 0)))) set
          else set

offsets_3 = [Pos [x, y, z] | x <- [-1..1], y <- [-1..1], z <- [-1..1], (x, y, z) /= (0, 0, 0)]
offsets_4 = [Pos [x, y, z, w] | x <- [-1..1], y <- [-1..1], z <- [-1..1], w <- [-1..1], (x, y, z, w) /= (0, 0, 0, 0)]

activeNeighbors :: [Pos] -> S.Set Pos -> M.Map Pos Int
activeNeighbors offsets = foldr processActive M.empty
  where processActive pos neighbor_count =
          foldr (processNeighbor pos) neighbor_count offsets
        processNeighbor pos offset neighbor_count =
          M.insertWith (+) (addPos pos offset) 1 neighbor_count

newActive :: S.Set Pos -> M.Map Pos Int -> S.Set Pos
newActive old =
  M.foldrWithKey maybeInsert S.empty
  where maybeInsert pos n s =
          let in_prev = S.member pos old
          in
            if (in_prev && 2 <= n && n <= 3) || ((not in_prev) && n == 3)
            then S.insert pos s
            else s

update :: [Pos] -> S.Set Pos -> S.Set Pos
update offsets old = newActive old (activeNeighbors offsets old)
