import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Maybe
import System.Environment

-- Observations:
--
-- 1. Every rule always expands into the same number of subrules, so
--    every full-parsed string will have the same length.

main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let [rulestrings, teststrings] = splitOn [""] (lines content)
      rules@(chr, ir) = parseRules rulestrings
      accepted_teststrings = filter (\t -> goodParse (parseString rules t 0)) teststrings

  putStrLn $ "Parse One, # accepted strings = " ++ (show (length accepted_teststrings))
             
goodParse  :: (String, Bool) -> Bool
goodParse parse@(rest, status) = rest == "" && status == True
             
parseRules :: [String] -> (M.Map Int Char, M.Map Int [[Int]])
parseRules = foldr parseOneRule (M.empty, M.empty)

parseOneRule :: String -> (M.Map Int Char, M.Map Int [[Int]]) -> (M.Map Int Char, M.Map Int [[Int]])
parseOneRule line r@(cr, ir) =
  let w = words line
      new_id = read (init (w !! 0))
  in if head (w !! 1) == '"'
     then (M.insert new_id ((w !! 1) !! 1) cr, ir)
     else (cr, M.insert new_id (parseIntLists (tail w)) ir)

parseIntLists :: [String] -> [[Int]]
parseIntLists [] = []
parseIntLists words = 
  let sublists = splitOn ["|"] words
  in map (map read) sublists

parseString :: (M.Map Int Char, M.Map Int [[Int]]) -> String -> Int -> (String, Bool)
parseString _ "" _ = ("", False)
parseString ruleset@(char_rules, int_rules) input@(h:rest) rule_id =
  if M.member rule_id char_rules
  then let jc@(Just c) = M.lookup rule_id char_rules
       in if c == h then (rest, True) else (input, False)
  else let jrl@(Just rule_lists) = M.lookup rule_id int_rules
           parses = map (parseStringWithIntRules ruleset input) rule_lists
           good_parse = find (\p@(_, good) -> good) parses
       in if isJust good_parse then fromJust good_parse else (input, False)

parseStringWithIntRules :: (M.Map Int Char, M.Map Int [[Int]]) -> String -> [Int] -> (String, Bool)
parseStringWithIntRules ruleset input intrules =
  foldl' (\pp@(rest, good) rule -> if not good then pp else parseString ruleset rest rule) (input, True) intrules
