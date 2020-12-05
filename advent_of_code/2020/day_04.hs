import Data.Char
import Data.Ix
import Data.List.Split
import qualified Data.Set as Set
import qualified Data.Map as Map
import System.Environment

main = do
  args <- getArgs
  content <- readFile (args !! 0)

  let password_lines = map words $ lines content
  let grouped = map concat $ splitWhen null password_lines

  putStrLn $ "# Valid: " ++ (show (length $ filter validPasswordP grouped))
  putStrLn $ "# Very Valid: " ++
    (show (length $ filter veryValidPasswordP grouped))

splitTerms :: String -> [String]
splitTerms = splitOn ":"

validPasswordP :: [String] -> Bool
validPasswordP terms_list =
  terms `Set.intersection` needed == needed
  where needed = Set.fromList ["byr", "iyr", "eyr", "hgt",
                               "hcl", "ecl", "pid"]
        terms = Set.fromList $ map (head . splitTerms ) terms_list
        
veryValidPasswordP :: [String] -> Bool
veryValidPasswordP terms_list =
  validPasswordP terms_list && 
  all (validTermP . (\h -> (h !! 0, h !! 1)) . splitTerms) terms_list

validTermP :: (String, String) -> Bool
validTermP ("byr", year) = validRangeP year 1920 2002
validTermP ("iyr", year) = validRangeP year 2010 2020
validTermP ("eyr", year) = validRangeP year 2020 2030
validTermP ("hgt", height) = validHeightP height
validTermP ("hcl", hair_color) = validHairColorP hair_color
validTermP ("ecl", eye_color) = validEyeColorP eye_color
validTermP ("pid", passport_id) = validPassportIdP passport_id
validTermP ("cid", _) = True
validTermP (_, _) = error "Unkown term."

validRangeP :: String -> Int -> Int -> Bool
validRangeP year_string low high = inRange (low, high) (read year_string)

validHeightP :: String -> Bool
validHeightP height =
  let (n, units) = splitAt ((length height) - 2) height
      result
        | units == "cm" = validRangeP n 150 193
        | units == "in" = validRangeP n 59 76
        | otherwise = False
  in result

validHairColorP :: String -> Bool
validHairColorP hair_color =
  head hair_color == '#' &&
  all (\e -> elem e "0123456789abcdef") (tail hair_color)

validEyeColorP :: String -> Bool
validEyeColorP eye_color =
  elem eye_color ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validPassportIdP :: String -> Bool
validPassportIdP passport_id =
  (length passport_id) == 9 && all isDigit passport_id
