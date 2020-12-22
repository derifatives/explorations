import Data.Foldable (toList)
import Data.List.Split
import Data.Sequence (Seq((:|>)), Seq(Empty, (:<|)))
import Data.Sequence as Seq hiding (reverse, zip)
import qualified Data.Set as S
import System.Environment

main = do
  args <- getArgs
  content <- readFile (args !! 0)

  let players =  splitOn [""] $ lines content
  let deck1 =  Seq.fromList $ map (\n -> read n :: Int) (tail (players !! 0))
      deck2 =  Seq.fromList $ map (\n -> read n :: Int) (tail (players !! 1))

  -- Part One
  let played_combat = combat (deck1, deck2)
  putStrLn $ "Combat Winning Score = " ++ (show (scoreCombatWinner played_combat))

  -- Part Two
  let played_recursive_combat = recursiveCombat (RC deck1 deck2 S.empty 0)
  putStrLn $ "Combat Winning Score = " ++ (show (scoreRecursiveCombatWinner played_recursive_combat))

scoreCombatWinner :: (Seq Int, Seq Int) -> Int
scoreCombatWinner decks =
  case decks of 
    (d, Empty) -> scoreDeck d
    (Empty, d) -> scoreDeck d

scoreRecursiveCombatWinner :: RecursiveCombatState -> Int
scoreRecursiveCombatWinner rc@(RC d1 _ _ 1) = scoreDeck d1
scoreRecursiveCombatWinner rc@(RC _ d2 _ 2) = scoreDeck d2

scoreDeck :: Seq Int -> Int
scoreDeck d = sum $ map (uncurry (*)) (zip (reverse $ toList d) [1..])

combat :: (Seq Int, Seq Int) -> (Seq Int, Seq Int)
combat (Empty, o) = (Empty, o)
combat (o, Empty) = (o, Empty)
combat (d1@(c1 :<| r1), d2@(c2 :<| r2)) =
  if c1 > c2
  then combat (r1 :|> c1 :|> c2, r2)
  else combat (r1, r2 :|> c2 :|> c1)

data CombatState = C (Seq Int) (Seq Int) deriving (Eq, Ord)
data RecursiveCombatState = RC (Seq Int) (Seq Int) (S.Set CombatState) Int

recursiveCombat :: RecursiveCombatState -> RecursiveCombatState
recursiveCombat rc@(RC Empty d2 s w) = RC Empty d2 s 2
recursiveCombat rc@(RC d1 Empty s w) = RC d1 Empty s 1
recursiveCombat rc@(RC d1@(c1 :<| r1) d2@(c2 :<| r2) s 0) =
  let cs = C d1 d2
  in if S.member cs s
     then RC d1 d2 s 1
     else let s' = S.insert cs s
              rw = if Seq.length r1 >= c1 && Seq.length r2 >= c2
                   then let rc@(RC _ _ _ w) =
                              recursiveCombat $ RC (Seq.take c1 r1) (Seq.take c2 r2) S.empty 0
                        in w
                   else if c1 > c2 then 1 else 2
          in case rw of
               1 -> recursiveCombat $ RC (r1 :|> c1 :|> c2) r2 s' 0
               2 -> recursiveCombat $ RC r1 (r2 :|> c2 :|> c1) s' 0
