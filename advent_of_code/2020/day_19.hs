import Data.List
import Data.List.Split
import Data.List.Unique
import qualified Data.Map.Strict as M
import Data.Maybe
import System.Environment

-- Design concerns:
--
-- We'll use [] to indicate a failure to parse. This is different from
-- [[]], which indicates we parsed the entire input.
main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let [rulestrings, teststrings] = splitOn [""] (lines content)
      rules@(chr, ir) = parseRules rulestrings
      parses =  (map (\t -> parseString rules t 0) teststrings)

  -- inputStrLn $ (show (accepted_teststrings))
  -- putStrLn $ (show (zip teststrings parses))
  putStrLn $ (show (length (filter goodParse parses)))
  --putStrLn $ (show (parseString rules "a" 4))
  -- putStrLn $ (show (parseString rules "aba" 2))
  -- putStrLn $ (show (parseString rules "ba" 4))

goodParse :: [String] -> Bool
goodParse = elem ""

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

parseString :: (M.Map Int Char, M.Map Int [[Int]]) -> String -> Int -> [String]
parseString _ "" _ = []
parseString ruleset@(char_rules, int_rules) input@(h:rest) rule_id =
  if M.member rule_id char_rules
  then let jc@(Just c) = M.lookup rule_id char_rules
       in if c == h then [rest] else []
  else let jrl@(Just rule_lists) = M.lookup rule_id int_rules
       in concat $ map (parseStringWithIntRules ruleset input) rule_lists

parseStringWithIntRules :: (M.Map Int Char, M.Map Int [[Int]]) -> String -> [Int] -> [String]
parseStringWithIntRules ruleset input [] = [input]
parseStringWithIntRules ruleset "" _ = []
parseStringWithIntRules ruleset input (rule:rest) =
  let rule_parses = unique $ parseString ruleset input rule
      in if null rule_parses
         then []
         else concat $ map (\r -> unique $ parseStringWithIntRules ruleset r rest) rule_parses
              
