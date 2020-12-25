import Data.Array.Unboxed
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import System.Environment

-- Tiles are stored row-major, top-row first (row zero is the top row, row nine is the bottom.)
-- We're not flipping the borders, so maybe don't include the borders in the tile? A wrapper?

ts = 10  -- Tile size, hard coded for convenience.
tt = ts-1
ts_bounds = ((0, 0), (ts-1, ts-1))

boundsToIndices :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
boundsToIndices ((by, bx), (ty, tx)) = [(y, x) | y <- [by..ty], x<-[bx..tx]]

topBound :: UArray (Int, Int) Char -> (Int, Int)
topBound arr = snd $ bounds arr

tile_arr_indices = boundsToIndices ts_bounds
tile_flip_indices = map (\(y, x) -> (y, tt-x)) tile_arr_indices
tile_rot_indices = map (\(y, x) -> (9-x, y)) tile_arr_indices

sea_monster_lines = [
  "                  # ",
  "#    ##    ##    ###",
  " #  #  #  #  #  #   "]

stringsToArray :: [String] -> UArray (Int, Int) Char
stringsToArray strings =
  let n_rows = length strings
      n_cols = length (head strings)
  in listArray ((0, 0), (n_rows-1, n_cols-1)) $ concat strings

arrayToCharLocs :: UArray (Int, Int) Char -> Char -> [(Int, Int)]
arrayToCharLocs arr char =
  map fst $ filter (((==) char) . snd) (assocs arr)

displayArray :: UArray (Int, Int) Char -> String
displayArray arr =
  let (_, max_col) = snd (bounds arr)
  in unlines $ chunksOf (max_col+1) (elems arr)

data Rotated = Unrot | Rot90 | Rot180 | Rot270 deriving (Eq, Show)

nextRot :: Rotated -> Rotated
nextRot Unrot = Rot90
nextRot Rot90 = Rot180
nextRot Rot180 = Rot270
nextRot Rot270 = Unrot

flipRot :: Rotated -> Rotated
flipRot Unrot = Unrot
flipRot Rot90 = Rot270
flipRot Rot180 = Rot180
flipRot Rot270 = Rot90

data Tile = Tile {tileId :: Int,
                  tileFlipped :: Bool,
                  tileRotated :: Rotated,
                  tileArray :: (UArray (Int, Int) Char)} deriving (Eq)

instance Ord Tile where
  (Tile t_id1 _ _ _) `compare` (Tile t_id2 _ _ _) = t_id1 `compare` t_id2
  
instance Show Tile where
  show (Tile t_id t_fl t_rot t_arr) =
    let id_str =  "Tile ID: " ++ (show t_id)
        fl_str = if t_fl then "Flipped" else "Unflipped"
        arr_str = displayArray t_arr
    in concat [id_str, " (", fl_str, ", ", (show t_rot), ")\n", arr_str]

tileConfigs :: Tile -> [Tile]
tileConfigs tile = scanl (\t f -> f t) tile fns
  where three_rotates = take 3 (repeat rotateTile)
        fns = three_rotates ++ ((flipTile . rotateTile) : three_rotates)

arrayConfigs :: UArray (Int, Int) Char -> [UArray (Int, Int) Char]
arrayConfigs arr = scanl (\a f -> f a) arr fns
  where three_rotates = take 3 (repeat rotateArray)
        fns = three_rotates ++ ((flipArray . rotateArray) : three_rotates)

parseTile :: [String] -> Tile
parseTile strings =
  let arr = listArray ts_bounds $ concat. tail $ strings
      int_id = read . init . head . tail . words . head $ strings
  in Tile int_id False Unrot arr

updateArray :: UArray (Int, Int) Char -> (Int, Int) -> [(Int, Int)] -> UArray (Int, Int) Char
updateArray arr upper_bound inds =
  listArray ((0, 0), upper_bound) (map ((!) arr) inds)

flipIndices :: UArray (Int, Int) Char -> [(Int, Int)]
flipIndices arr =
  let (_, max_cols) = topBound arr
  in map (\(y, x) -> (y, max_cols-x)) $ indices arr

flipArray :: UArray (Int, Int) Char -> UArray (Int, Int) Char
flipArray arr = updateArray arr (topBound arr) (flipIndices arr)

rotateIndices :: UArray (Int, Int) Char -> [(Int, Int)]
rotateIndices arr =
  let (nr, nc) = topBound arr
      new_inds = [(y, x) | y<-[0..nc], x<-[0..nr]]
  in map (\(y, x) -> (nr-x,y)) $ new_inds

rotateArray :: UArray (Int, Int) Char -> UArray (Int, Int) Char
rotateArray arr =
  updateArray arr ((uncurry $ flip (,)) (topBound arr)) (rotateIndices arr)

flipTile :: Tile -> Tile
flipTile (Tile t_id t_fl t_rot t_arr) =
  Tile t_id (not t_fl) (flipRot t_rot) (updateArray t_arr (snd (bounds t_arr))  tile_flip_indices)

rotateTile :: Tile -> Tile
rotateTile (Tile t_id t_fl t_rot t_arr) =
  Tile t_id t_fl (nextRot t_rot) (updateArray t_arr (snd (bounds t_arr)) tile_rot_indices)

data Direction = North | East | South | West deriving (Show, Eq)

directionToOffset :: Direction -> (Int, Int)
directionToOffset North = (0, 1)
directionToOffset East = (1, 0)
directionToOffset South = (0, -1)
directionToOffset West = (-1, 0)

directions = [North, East, South, West]
directionOffsets = map directionToOffset directions

instance Ord Direction where
  compare d1 d2 = compare (rank d1) (rank d2) where
    rank North = 0
    rank East = 1
    rank South = 2
    rank West = 3

matchingDirection :: Direction -> Direction
matchingDirection North = South
matchingDirection East = West
matchingDirection South = North
matchingDirection West = East

grabBorder :: Tile -> Direction -> [Char]
grabBorder tile direction = map ((!) (tileArray tile)) direction_inds
  where direction_inds = case direction of
          North -> [(0, i)  | i <- [0..tt]]
          East  -> [(i, tt) | i <- [0..tt]]
          South -> [(tt, i) | i <- [0..tt]]
          West  -> [(i, 0)  | i <- [0..tt]]
              
data ImageState = IS {
  -- "Raw" tiles we left to place. Each can be in any configuration.
  remainingTiles :: S.Set Tile,
  -- "Configured" tiles already placed, by position.
  placedTiles    :: M.Map (Int, Int) Tile,
  -- Representation for areas that are ready to place. The desired
  -- invariant position is if p->(t, d) is a key-value pair in wanted,
  -- then t is in placedTiles at a position p', s.t.
  --    p' + directionToOffset d = p
  wanted         :: M.Map (Int, Int) (Tile, Direction)} deriving (Show)

stateSummary :: ImageState -> String
stateSummary (IS rt pt w) = (show (M.size pt)) ++ " placed tiles, " ++ (show (S.size rt)) ++ " remaining tiles, " ++ (show (M.size w)) ++ " wanted placements."

initialImageState :: [Tile] -> ImageState
initialImageState tiles =
  let first_tile = head tiles
      first_pos = (0, 0)
      placed_tiles = M.singleton first_pos first_tile
  in IS (S.fromList $ tail tiles) placed_tiles (addWanted placed_tiles M.empty first_pos)

tileBounds :: M.Map (Int, Int) Tile -> ((Int, Int), (Int, Int))
tileBounds pt =
  foldr (\k@(x, y) ((min_x, min_y), (max_x, max_y)) ->
           ((min x min_x, min y min_y), (max x max_x, max y max_y))) ((0, 0), (0, 0)) (M.keys pt)
  
tryToAddTile :: ImageState -> ImageState
tryToAddTile is@(IS rt pt w) =
  if w == M.empty then is else
    let (wanted_pos@(wx, wy), (prev_tile, direction)) = head $ M.toList w
        maybe_new_tile = findMatchingTile rt prev_tile direction
        new_wanted = M.delete wanted_pos w
    in if isNothing maybe_new_tile
       then IS rt pt new_wanted
       else
         let new_tile = fromJust maybe_new_tile
             new_rt = S.delete new_tile rt
             new_pt = M.insert wanted_pos new_tile pt
             new_wanted' = addWanted new_pt new_wanted wanted_pos
         in IS new_rt (M.insert wanted_pos new_tile pt) new_wanted'

addWanted :: M.Map (Int, Int) Tile -> M.Map (Int, Int) (Tile, Direction) -> (Int, Int) -> M.Map (Int, Int) (Tile, Direction)
addWanted placed_tiles wanted last_pos@(px, py) =
  let last_tile = fromJust $ M.lookup last_pos placed_tiles
      addFn dir w =
        let o@(ox, oy) = directionToOffset dir
            newpos = (px+ox, py+oy)
        in if M.member newpos w
           then w
           else M.insert newpos (last_tile, dir) w
  in foldr addFn wanted directions
             
findMatchingTile :: S.Set Tile -> Tile -> Direction -> Maybe Tile
findMatchingTile tileset tile dir =
  let tile_border = grabBorder tile dir
      matchdir = matchingDirection dir
      matchP candidate = grabBorder candidate matchdir == tile_border
  in find matchP $ concat (map tileConfigs $ S.toList tileset)

-- Note: linked_tiles has increasing tiles up, and (x, y)
-- coordinates. The image array has increasing tiles down, and (y, x)
-- coordinates.
assembleImage :: M.Map (Int, Int) Tile -> UArray (Int, Int) Char
-- assembleImage :: M.Map (Int, Int) Tile -> (((Int, Int), (Int, Int)), [[((Int, Int), Char)]])
assembleImage linked_tiles =
  let ((min_x, min_y), (max_x, max_y)) = tileBounds linked_tiles
      x_tilediff = max_x - min_x
      y_tilediff = max_y - min_y
      spots_per_tile = ts - 2
      x_top = ((x_tilediff + 1) * spots_per_tile) - 1
      y_top = ((y_tilediff + 1) * spots_per_tile) - 1
      image_bounds = ((0, 0), (y_top, x_top))
      tile_list = M.toList linked_tiles
      per_tile_fn (tile_pos@(tx, ty), tile@(Tile _ _ _ arr)) =
        let filter_pos_fn ((y, x), c) =
              y /= 0 && y /= tt && x /= 0 && x /= tt
            per_pos_fn ((y, x), c) =
              -- Note deliberate swap of coordinates and adjustment for border removal.
              (((max_y - ty) * spots_per_tile + y - 1, (tx - min_x) * spots_per_tile + x - 1), c)
        in map per_pos_fn (filter filter_pos_fn (assocs arr))
      elts = map per_tile_fn tile_list
  in accumArray (\_ c -> c) 'X' image_bounds $ concat (map per_tile_fn tile_list)

offsetsMatching :: UArray (Int, Int) Char -> UArray (Int, Int) Char -> [(Int, Int)]
offsetsMatching big small =
  let (_, (big_y, big_x)) = bounds big
      (_, (small_y, small_x)) = bounds small
      max_y_offset = big_y - small_y
      max_x_offset = big_x - small_x
      offsets = [(y, x) | y <- [0..max_y_offset], x <- [0..max_x_offset]]
      locs = arrayToCharLocs small '#'
      check_offset (oy, ox) =
        all (\(ly, lx) -> (big ! (oy+ly, ox+lx)) == '#') locs
  in filter check_offset offsets
  
main = do
  args <- getArgs
  content <- readFile (args !! 0)

  let tiles = map parseTile (splitOn [""] (lines content))
      tile_ids = map tileId tiles
      init_state = initialImageState tiles
      linked_tiles = fromJust $ find (\is@(IS _ _ w) -> M.size w == 0) (iterate tryToAddTile init_state)
      placed_tiles = placedTiles linked_tiles
      sea_monster = stringsToArray sea_monster_lines
      matched_image = rotateArray $ assembleImage placed_tiles
      total_waves = length $ filter (== '#') (elems matched_image)
      sea_monster_size = length $ arrayToCharLocs sea_monster '#'
      num_monsters = length (fromJust (find (not . null) (map (offsetsMatching matched_image) (arrayConfigs sea_monster))))
  putStrLn $ "Total waves: " ++ (show total_waves)
  putStrLn $ "Roughness: " ++ (show (total_waves - num_monsters * sea_monster_size))
