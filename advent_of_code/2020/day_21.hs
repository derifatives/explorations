import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Data.Sort
import System.Environment

main = do
  args <- getArgs
  content <- readFile (args !! 0)

  let food_lines = lines content
      parsed = map parseFoodLine food_lines
      initial_intersections = initialIntersections parsed
  putStrLn $ show initial_intersections
  putStrLn $ "# Allergens == " ++ (show $ M.size initial_intersections)
  let allergens = M.foldr S.union S.empty initial_intersections
  putStrLn $ "# Of Ingredients in `Initial Allergens` = " ++ (show $ S.size allergens)
  -- Since these are the same, everything in this intersection is an
  -- allergen, everything else isn't.
  
  let all_ingredients = foldr S.union S.empty (map fst parsed)
  putStrLn $ "Total # of Ingredients = " ++ (show $ S.size all_ingredients)
  let non_allergens = all_ingredients S.\\ allergens
  putStrLn $ "Total # of non-allergens = " ++ (show $ S.size non_allergens)

  let total_count = sum $ map ((\s -> S.size $ S.intersection s non_allergens) . fst) parsed
  putStrLn $ "Total # of appearances of non-allergens = " ++ (show total_count)

  -- Part Two
  let ((k, _):[]) = M.toList $ M.filter (\s -> S.size s == 1) initial_intersections
      prematching = findMatching (initial_intersections, [k])
      matching = map (\(k, s) -> (k, head $ S.toList s)) (M.toList $ fst prematching)
      sorted = (sortBy (\e1 e2 -> if (fst e1) < (fst e2) then LT else GT) matching)
  putStrLn $ show (intercalate "," (map snd sorted))

parseFoodLine :: String -> (S.Set String, S.Set String)
parseFoodLine line =
  let (ingredients : (preallergens : [])) = splitOn ["(contains"] $ words line
      allergens = map init preallergens -- Remove last char.
  in (S.fromList ingredients, S.fromList allergens)

initialIntersections :: [(S.Set String, S.Set String)] -> M.Map String (S.Set String)
initialIntersections foods_and_allergens =
  foldr addFoodToIntersectionMap M.empty foods_and_allergens

addFoodToIntersectionMap :: (S.Set String, S.Set String) -> M.Map String (S.Set String) -> M.Map String (S.Set String)
addFoodToIntersectionMap f@(food_set, allergens) allergen_map =
  foldr (\a m -> M.insertWith S.intersection a food_set m) allergen_map (S.toList allergens)


findSingletonKeys :: M.Map String (S.Set String) -> S.Set String
findSingletonKeys ms =  M.foldrWithKey (\k a s -> if (S.size a) == 1 then S.insert k s else s) S.empty  ms

findKeysContaining :: M.Map String (S.Set String) -> String -> S.Set String
findKeysContaining ms t = M.foldrWithKey (\k a s -> if (S.member t a) then S.insert k s else s) S.empty ms

findMatching :: (M.Map String (S.Set String), [String]) -> (M.Map String (S.Set String), [String])
findMatching (ms, []) = (ms, [])
findMatching (ms, sk:q) =
  let sv = head (S.toList $ fromJust (M.lookup sk ms))
      other = S.delete sk $ findKeysContaining ms sv
      ms' = foldr (\o m -> M.adjust (\s -> S.delete sv s) o m) ms other
      singletons = findSingletonKeys ms'
      q' = foldr (\o q -> if S.member o singletons then o:q else q) q other
  in findMatching (ms', q')
