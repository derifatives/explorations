import Data.Foldable
import Data.List
import qualified Data.Sequence as S
import System.Environment

main = do
  args <- getArgs
  content <- readFile (args !! 0)

  let ns = map (\l -> read l :: Int) (lines content)
      pn = 25
      slices l = (p, head r):(slices (tail l)) where (p, r) = splitAt pn l
      f = \(p, n) -> null [(x,y) | x<-p, y<-p, x+y==n, x /= y]
      target = snd (head (filter f (slices ns)))
      
  putStrLn $ show "Target: target"

  let sum_state = findSeqWithSum (D S.empty 0) ns target
  putStrLn $ "Sum state:" ++ (show sum_state)
  let s = toList (sq sum_state) in
    putStrLn $ "Weakness: " ++ show ((minimum s) + (maximum s))

data D = D (S.Seq Int) Int deriving (Show)

sq :: D -> S.Seq Int
sq d@(D seq _) = seq

findSeqWithSum :: D -> [Int] -> Int -> D
findSeqWithSum d@(D s t) ns tr
  | t == tr   = d
  | t < tr    = let h = (head ns) in findSeqWithSum (D (s S.|> h) (t + h)) (tail ns) tr
  | t > tr    = let f = S.index s 0 in findSeqWithSum (D (S.drop 1 s) (t - f)) ns tr
