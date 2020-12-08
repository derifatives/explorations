import qualified Data.Map.Strict as Map
import System.Environment

main = do
  args <- getArgs
  content <- readFile (args !! 0)

  let remove_words = ["bag", "bags", "bag,", "bags,", "bag.", "bags.", "no", "other", "contain"]
      colors_and_numbers = map ((filter (\l -> not (elem l remove_words))) . words) (lines content)
      parsed_colors_and_numbers = map parseColors colors_and_numbers
      parsed_colors = map (\can->(fst can, map snd (snd can))) parsed_colors_and_numbers
      graph = buildMap parsed_colors
  
  putStrLn $ "Colors that could contain 'shiny gold': " ++ show (length (search graph "shiny gold"))
  putStrLn $ "Number of bags in a 'shiny gold': " ++ show ((countBags (Map.fromList parsed_colors_and_numbers) "shiny gold") - 1)

color :: String -> String -> String
color a c = a ++ " " ++ c

parseColors :: [String] -> (String, [(Int, String)])
parseColors (a:c:rest) = (color a c, parseChildColors rest)
parseColors _ = error "Bad color line."

parseChildColors :: [String] -> [(Int, String)]
parseChildColors [] = []
parseChildColors (n:a:c:rest) = (read n, color a c):parseChildColors rest
parseChildColors _ = error "Bad color line."

buildMap :: [(String, [String])] -> Map.Map String [String]
buildMap = foldr buildMapOneLine Map.empty

buildMapOneLine :: (String, [String])  -> Map.Map String [String] -> Map.Map String [String]
buildMapOneLine (bag, contained) m =
  foldr (\c m -> Map.insertWith (++) c [bag] m) m contained

search :: Map.Map String [String] -> String -> [String]
search graph key = fst (search' graph ([], next))
  where next = Map.findWithDefault [] key graph

search' :: Map.Map String [String] -> ([String], [String]) -> ([String], [String])
search' graph (found, []) = (found, [])
search' graph (found, queue@(q1:qr)) =
  if elem q1 found
  then search' graph (found, qr)
  else let newfound = (Map.findWithDefault [] q1 graph)
       in search' graph (q1:found, qr ++ newfound)

-- Non-working depth first search.
-- search' :: Map.Map String [String] -> String -> [String] -> [String]
-- search' graph key already_found =
--   foldr (\c af -> if elem c af then af else search' graph c (c:already_found)) already_found (Map.findWithDefault [] key graph)

countBags :: Map.Map String [(Int, String)] -> String -> Int
countBags m k =
  1 + sum (map (\(n, kk)->n * countBags m kk) (Map.findWithDefault [] k m))
