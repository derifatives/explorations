import Data.Array.Unboxed
import Data.List
import Data.List.Split
import System.Environment

ts = 10  -- Tile size, hard coded for convenience.

main = do
  args <- getArgs
  content <- readFile (args !! 0)

  let tiles = map parseTile (splitOn [""] (lines content))
      matchcounts = map (countMatches tiles) tiles
      tile_ids = map tile_id tiles
  putStrLn $ (show (product . map snd $ filter ((== 4) . fst) $ zip matchcounts tile_ids))
  
data Tile = Tile {tile_id :: Int,
                  tile_array :: (UArray (Int, Int) Bool),
                  tile_border :: [Border]} deriving (Show, Eq)
data Border = B (UArray Int Bool) deriving (Show, Eq)

parseTile :: [String] -> Tile
parseTile strings =
  let arr = listArray ((0, 0), (ts-1, ts-1)) $ map ((==) '#') (concat .tail $ strings)
      int_id = read . init . head . tail . words . head $ strings
  in Tile int_id arr (getBorders arr)

getBorders :: (UArray (Int, Int) Bool) -> [Border]
getBorders arr =
  map (\il -> B $ listArray (0, ts-1) $ map (\t -> arr ! t) il)
  [ zip z up, zip z down, zip up z, zip down z,
    zip top up, zip top down, zip up top, zip down top]
  where
    up = [0..(ts-1)]
    down = reverse up
    z = repeat 0
    top = repeat (ts-1)

countMatches :: [Tile] -> Tile -> Int
countMatches all_tiles tile@(Tile _ _ tb) =
  let tbc = map (==) tb
      adjmap ot@(Tile _ _ ob) = if ot == tile then [] else tbc <*> ob
  in sum $ map (length . filter (== True) . adjmap) all_tiles
