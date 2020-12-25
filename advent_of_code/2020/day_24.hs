import qualified Data.Map as M
import qualified Data.Set as S
import System.Environment

main = do
  args <- getArgs
  content <- readFile (args !! 0)

  let dirs = map parseString (lines content)
      initial_set = foldr (\dl s -> update (finalLocation dl) s) S.empty dirs
  putStrLn $ "Initial Size: " ++ (show $ S.size initial_set)
  putStrLn $ "After 100 days: " ++ (show $ S.size ((iterate dailyUpdate initial_set) !! 100))
  
data Dir = NE | E | SE | SW |  W | NW deriving (Show)
directions = [NE, E, SE, SW, W, NW]

data Pos = P Int Int deriving (Show, Eq)
instance Ord Pos where (P x1 y1) `compare` (P x2 y2) = (x1, y1) `compare` (x2, y2)

parseString :: String -> [Dir]
parseString [] = []
parseString ('n':('e':rest)) = NE : parseString rest
parseString ('e':rest) = E : parseString rest
parseString ('s':('e':rest)) = SE : parseString rest
parseString ('s':('w':rest)) = SW : parseString rest
parseString ('w':rest) = W : parseString rest
parseString ('n':('w':rest)) = NW : parseString rest

newPos :: Dir -> Pos -> Pos
newPos NE (P x y) = if mod y 2 == 0 then (P x (y+1)) else (P (x+1) (y+1))
newPos  E (P x y) = P (x+1) y
newPos SE (P x y) = if mod y 2 == 0 then (P x (y-1)) else (P (x+1) (y-1))
newPos SW (P x y) = if mod y 2 == 1 then (P x (y-1)) else (P (x-1) (y-1))
newPos  W (P x y) = (P (x-1) y)
newPos NW (P x y) = if mod y 2 == 1 then (P x (y+1)) else (P (x-1) (y+1))

start = P 0 0

finalLocation :: [Dir] -> Pos
finalLocation = foldr newPos start

update :: Pos -> S.Set Pos -> S.Set Pos
update pos s = if S.member pos s then S.delete pos s else S.insert pos s

neighorMap :: S.Set Pos -> M.Map Pos Int
neighorMap pos_set =
  let all_neighbors = concat (map (\p -> map ((flip newPos) p) directions) (S.toList pos_set))
  in foldr (\n m -> M.insertWith (+) n 1 m) M.empty all_neighbors

newFlippedSet :: M.Map Pos Int -> S.Set Pos -> S.Set Pos
newFlippedSet neighbor_map flipped =
  let numNeighbors p = M.findWithDefault 0 p neighbor_map
      unflipped_neighbors = M.filterWithKey (\p _ -> not $ S.member p flipped) neighbor_map
      addIfNewlyFlipped p n s = if n == 2 then S.insert p s else s
      newly_flipped = M.foldrWithKey addIfNewlyFlipped S.empty unflipped_neighbors  
      add_old_flipped = S.foldr (\p s -> let n = numNeighbors p in if n == 0 || n > 2 then s else S.insert p s) newly_flipped flipped
  in add_old_flipped

dailyUpdate :: S.Set Pos -> S.Set Pos
dailyUpdate s = newFlippedSet (neighorMap s) s
