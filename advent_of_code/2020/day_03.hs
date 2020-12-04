import System.Environment

main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let terrain_checker = numberOfTrees (lines content)

  putStrLn $ "Part 1 # Trees: " ++ (show (terrain_checker 3 1))

  let directions = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  let part_two_product = product $ map (uncurry terrain_checker) directions
  putStrLn $ "Part 2 # Product: " ++ (show part_two_product)
          
takeFirstAndEveryNth :: Int -> [a] -> [a]
takeFirstAndEveryNth n = map snd . filter ((==1) . fst) . zip (cycle [1..n])

numberOfTrees :: [String] -> Int -> Int -> Int
numberOfTrees lines right down =
  let filtered_lines = takeFirstAndEveryNth down lines
  in length $
     filter (== '#') (map (\(l, n) -> l !! n)
                       (zip (map cycle filtered_lines) [0, right..]))
