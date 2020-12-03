import Data.List.Split
import System.Environment

main = do
  args <- getArgs
  content <- readFile (args !! 0)

  let num_valid = length $ filter validP (lines content)
  putStrLn $ "# Valid Passwords, Old Policy: " ++ (show num_valid)

  let num_valid_new = length $ filter validPNew (lines content)
  putStrLn $ "# Valid Passwords, New Policy: " ++ (show num_valid_new)

validP :: String -> Bool
validP policy_and_password =
  let [range, key_char:_, password] = words policy_and_password
      count = length $ filter (== key_char) password
      [low, high] = map read $ splitOn "-" range
  in low <= count && count <= high

validPNew :: String -> Bool
validPNew policy_and_password =
  let [range, key_char:_, password] = words policy_and_password
      [p1, p2] = map ((subtract 1) . read) $ splitOn "-" range
  in (password !! p1 == key_char) /= (password !! p2 == key_char)
  
