import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Environment

main = do
  args <- getArgs
  content <- readFile (args !! 0)

  let lls = lines content
      (field_lines, lls_2) = getFieldLines lls
      fields = map parseFieldLine field_lines
      (your_ticket, lls_3) = parseYourTicket lls_2
      nearby_tickets = parseNearbyTickets lls_3

  -- Part One
  let invalidTicketNums = filter (noValidFieldPositionsInt fields)
  putStrLn $ "Part One: " ++ (show (sum (map (sum . invalidTicketNums) nearby_tickets)))

  -- Part Two
  let filtered_tickets = filter (all (not . noValidFieldPositionsInt fields)) nearby_tickets
      vfp = validFieldPositionsAllTickets fields filtered_tickets
  putStrLn $ (show (find (\s -> S.size s == 1) vfp))

  let valid_positions = validFieldPositionsAllTickets fields filtered_tickets
      -- Matching is position -> field
      matching = findPositionFieldMatching valid_positions []
      field_positions = map fst (sortOn snd matching)
      departure_fields = map snd $ filter (\(f@(Field name _), _) -> isPrefixOf "departure" name) (zip fields [0..])
      result = map (\dp -> your_ticket !! (field_positions !! dp)) departure_fields
  putStrLn $ (show (result, product result))
  
data Field = Field String [(Int, Int)] deriving (Show)

getFieldLines :: [String] -> ([String], [String])
getFieldLines = span (not . null)

parseTicket :: String -> [Int]
parseTicket = map read . splitOn ","

parseYourTicket :: [String] -> ([Int], [String])
parseYourTicket lls =
  let (_, (_:rest)) = span ((/=) "your ticket:") lls
  in (parseTicket (head rest), tail rest)

parseNearbyTickets :: [String] -> [[Int]]
parseNearbyTickets lls =
  let (_, (_:rest)) = span ((/=) "nearby tickets:") lls
  in map parseTicket rest

parseFieldLine :: String -> Field
parseFieldLine line =
  let (name, rest) = span (not . (== ':')) line
      ranges = map rangeFromWord (filter (/= "or") (words (tail rest)))
  in Field name ranges
  where
    rangeFromWord w =
      let l = map read (splitOn "-" w) in (l !! 0, l !! 1)

isValidForField :: Int -> Field -> Bool
isValidForField n f@(Field _ rr) = any (isValidForRange n) rr

isValidForRange :: Int -> (Int, Int) -> Bool
isValidForRange n r@(l, h) = l <= n && n <= h

noValidFieldPositionsInt :: [Field] -> Int -> Bool
noValidFieldPositionsInt fields n = validFieldPositionsInt fields n == S.empty

validFieldPositionsInt :: [Field] -> Int -> S.Set Int
validFieldPositionsInt fields n = foldl' fn S.empty (zip fields [0..])
  where fn = \s (f, i) -> if isValidForField n f
                          then S.insert i s
                          else s

validFieldPositionsAllTickets :: [Field] -> [[Int]] -> [S.Set Int]
validFieldPositionsAllTickets fields tickets =
  let num_positions = length fields
      positions = [0..(num_positions-1)]
      init_val = take num_positions (repeat (S.fromList positions))
  in foldl' (\s t -> map (uncurry S.intersection) (zip s (map (validFieldPositionsInt fields) t))) init_val tickets

findPositionFieldMatching :: [S.Set Int] -> [(Int, Int)] -> [(Int, Int)]
findPositionFieldMatching valid_positions matches =
  if (length valid_positions) == (length matches) then matches
  else
    let Just (field_set, position) = find (\(fs, _) -> S.size fs == 1) (zip valid_positions [0..])
        field = head (S.toList field_set)
    in findPositionFieldMatching (map (S.delete field) valid_positions) ((position, field):matches)
