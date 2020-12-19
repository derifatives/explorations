import Data.Char
import System.Environment

main = do
  args <- getArgs
  content <- readFile (args !! 0)

  -- Part One
  let results = map evalFormula (lines content)
  putStrLn $ "Part One: " ++ (show (sum results))

  let results2 = map evalFormula2 (lines content)
  putStrLn $ "Part Two: " ++ (show (sum results2))

data Op = Plus | Times deriving (Show)
data St = St (Maybe Int) (Maybe Op) String deriving (Show)

opToFn :: Op -> (Int -> Int -> Int)
opToFn Plus = (+)
opToFn Times = (*)

evalFormula :: String -> Int
evalFormula s =
  let st@(St (Just n) _ _) = evalFormula' (St Nothing Nothing s)
  in n

evalFormula' :: St -> St
evalFormula' st@(St (Just val) Nothing "") = st
evalFormula' st@(St mi mo (' ':rest)) = evalFormula' (St mi mo rest)
evalFormula' st@(St mi@(Just _) Nothing ('+':rest)) = evalFormula' (St mi (Just Plus) rest)
evalFormula' st@(St mi@(Just _) Nothing ('*':rest)) = evalFormula' (St mi (Just Times) rest)
evalFormula' st@(St Nothing Nothing ('(':rest)) =
  evalFormula' $ evalFormula' (St Nothing Nothing rest)
evalFormula' st@(St (Just prev) (Just op) ('(':rest)) = 
  let st@(St (Just subval) Nothing cont) = evalFormula' (St Nothing Nothing rest) in
    evalFormula' (St (Just ((opToFn op) prev subval)) Nothing cont)
evalFormula' st@(St (Just val) Nothing (')':rest)) = St (Just val) Nothing rest
evalFormula' st@(St Nothing Nothing (c:rest)) =
  evalFormula' (St (Just (digitToInt c)) Nothing rest)
evalFormula' st@(St (Just prev) (Just op) (c:rest)) =
  evalFormula' (St (Just ((opToFn op) prev (digitToInt c))) Nothing rest)

evalFormula2 :: String -> Int
evalFormula2 s =
  let st@(St (Just n) _ _) = evalFormula2' (St Nothing Nothing s)
  in n

evalFormula2' :: St -> St
evalFormula2' st@(St (Just val) Nothing "") = st
evalFormula2' st@(St mi mo (' ':rest)) = evalFormula2' (St mi mo rest)
evalFormula2' st@(St mi@(Just _) Nothing ('+':rest)) = evalFormula2' (St mi (Just Plus) rest)
evalFormula2' st@(St mi@(Just _) Nothing ('*':rest)) = evalFormula2' (St mi (Just Times) rest)
evalFormula2' st@(St Nothing Nothing ('(':rest)) =
  evalFormula2' $ evalFormula2' (St Nothing Nothing rest)
evalFormula2' st@(St (Just prev) (Just Plus) ('(':rest)) = 
  let st@(St (Just subval) Nothing cont) = evalFormula2' (St Nothing Nothing rest) in
    evalFormula2' (St (Just (prev + subval)) Nothing cont)
evalFormula2' st@(St (Just prev) (Just Times) ('(':rest)) = 
  let st@(St (Just subval) Nothing cont) = evalFormula2' (St Nothing Nothing ('(':rest)) in
    St (Just (prev * subval)) Nothing cont
evalFormula2' st@(St (Just val) Nothing (')':rest)) = St (Just val) Nothing rest
evalFormula2' st@(St Nothing Nothing (c:rest)) =
  evalFormula2' (St (Just (digitToInt c)) Nothing rest)
evalFormula2' st@(St (Just prev) (Just Plus) (c:rest)) =
  evalFormula2' (St (Just (prev + (digitToInt c))) Nothing rest)
evalFormula2' st@(St (Just prev) (Just Times) rest) =
  let st@(St (Just subval) Nothing cont) = evalFormula2' (St Nothing Nothing rest) in
    St (Just (prev * subval)) Nothing cont
