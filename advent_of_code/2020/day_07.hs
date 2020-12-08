import qualified Data.Map.Strict as Map
import System.Environment

main = do
  args <- getArgs
  content <- readFile (args !! 0)

  let remove_words = ["bag", "bags", "bag,", "bags,", "bag.", "bags.", "no", "other", "contain"]
      colors_and_numbers = map ((filter (\l -> not (elem l remove_words))) . words) (lines content)
      parsed_colors_and_numbers = map parseColors colors_and_numbers
      graph = buildGraph parsed_colors_and_numbers
  
  putStrLn $ "Colors that could contain 'shiny gold': " ++ show (length (search graph "shiny gold"))
  putStrLn $ "Number of bags in a 'shiny gold': " ++ show ((countBags (Map.fromList parsed_colors_and_numbers) "shiny gold") - 1)

type Node = (Int, String)
type Graph = Map.Map String [String]

color :: String -> String -> String
color a c = unwords [a, c]

parseColors :: [String] -> (String, [Node])
parseColors (a:c:rest) = (color a c, parseChildColors rest)
parseColors _ = error "Bad color line."

parseChildColors :: [String] -> [Node]
parseChildColors [] = []
parseChildColors (n:a:c:rest) = (read n, color a c):parseChildColors rest
parseChildColors _ = error "Bad color line."

buildGraph :: [(String, [Node])] -> Graph
buildGraph = foldr buildGraph' Map.empty

buildGraph' :: (String, [Node])  -> Graph -> Graph
buildGraph' (bag, contained) m =
  foldr (\c@(n, cl) m -> Map.insertWith (++) cl [bag] m) m contained

search :: Graph -> String -> [String]
search graph key = fst (search' graph ([], next))
  where next = Map.findWithDefault [] key graph

search' :: Graph -> ([String], [String]) -> ([String], [String])
search' graph (found, []) = (found, [])
search' graph (found, queue@(q1:qr)) =
  if elem q1 found
  then search' graph (found, qr)
  else let newfound = (Map.findWithDefault [] q1 graph)
       in search' graph (q1:found, qr ++ newfound)

countBags :: Map.Map String [Node] -> String -> Int
countBags m k =
  1 + sum (map (\(n, kk)->n * countBags m kk) (Map.findWithDefault [] k m))
