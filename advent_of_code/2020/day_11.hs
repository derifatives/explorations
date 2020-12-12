import Data.List
import qualified Data.Map.Strict as M
import System.Environment
import Data.List

main = do
  args <- getArgs
  content <- readFile (args !! 0)

  let strings = lines content
      ferry = ferryFromStrings strings (L ((length strings) - 1,
                                            (length (head strings)) - 1))

  let updater = update directionAdj 4 in
    putStrLn $ "Num occupied, Adjacent: " ++ (show (numOccupiedSeats (findFix updater ferry)))

  let updater = update directionRay 5 in
    putStrLn $ "Num occupied, Ray: " ++ (show (numOccupiedSeats (findFix updater ferry)))

data Loc = L (Int, Int) deriving (Eq, Ord, Show)
data Val = Empty | Floor | Occupied | OutOfBounds deriving (Eq, Show)
data Ferry = F (M.Map Loc Val) Loc deriving (Eq)

instance Show Ferry where
  show ferry@(F m (L s@(max_r, _))) =
    unlines $ ("Ferry max loc=" ++ (show s)) : (map (buildRowString ferry) [0..max_r])

numOccupiedSeats :: Ferry -> Int
numOccupiedSeats (F m s) = M.foldr' (\v t -> t + if v == Occupied then 1 else 0) 0 m

buildRowString :: Ferry -> Int -> String
buildRowString (F m (L s@(_, max_c)))  row = map (\c -> valToChar (M.lookup (L (row, c)) m)) [0..max_c]

valToChar :: Maybe Val -> Char
valToChar (Just Empty) = 'L'
valToChar (Just Floor) = '.'
valToChar (Just Occupied) = '#'
valToChar Nothing = 'X'

buildVal :: Char -> Val
buildVal c
  | c == 'L' = Empty
  | c == '.' = Floor

ferryFromStrings :: [String] -> Loc -> Ferry
ferryFromStrings lines size = ferryFromStrings' lines (L (0, 0)) (F M.empty size)

ferryFromStrings' :: [String] -> Loc -> Ferry -> Ferry
ferryFromStrings' [] _ f = f
ferryFromStrings' ([]:l) (L (r,_)) f = ferryFromStrings' l (L (r+1, 0)) f
ferryFromStrings' ((v:vv):l) loc@(L (r, c)) (F m s) = ferryFromStrings' (vv:l) (L (r, c+1)) (F (M.insert loc (buildVal v) m) s)

update :: (Ferry -> Loc -> (Int, Int) -> Val) -> Int -> Ferry -> Ferry
update directionFn maxocc f@(F m s) = F (M.mapWithKey (updateLoc directionFn maxocc f) m) s

updateLoc :: (Ferry -> Loc -> (Int, Int) -> Val) -> Int -> Ferry -> Loc -> Val -> Val
updateLoc _ _ _ _ Floor = Floor
updateLoc directionFn maxocc f l oldval
  | oldval == Empty &&  n_occ_near == 0         = Occupied
  | oldval == Occupied && n_occ_near >= maxocc  = Empty
  | otherwise                                   = oldval
  where
    n_occ_near = sum [if directionFn f l (y, x) == Occupied then 1 else 0 | y <- [-1..1], x <- [-1..1], x /= 0 || y /=  0]

directionAdj :: Ferry -> Loc -> (Int, Int) -> Val
directionAdj (F m _) (L (r, c)) (y, x) = M.findWithDefault OutOfBounds (L (r+y, c+x)) m

directionRay :: Ferry -> Loc -> (Int, Int) -> Val
directionRay (F m _) (L (r, c)) (y, x) =
  head (filter (/= Floor) (
           map (\n -> M.findWithDefault OutOfBounds (L (r+n*y, c+n*x)) m) [1..]))
        
findFix :: (Eq a) => (a->a) -> a -> a
findFix f a =
  let updates = iterate f a
      zipped = zip updates (tail updates)
  in fst $ head (filter (uncurry (==)) zipped)
