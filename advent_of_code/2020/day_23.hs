import Data.Foldable
import Data.List.Split
import Data.Sequence as S
import System.Environment

-- We keep the current cup in front.

main = do
  let initial_config = fromList $ map (\c -> read [c]) "712643589" :: Seq Int

  -- Part One
  let part_one_final_config = (iterate (move 9) initial_config) !! 100      
  putStrLn $ show (assembleAnswer part_one_final_config)

move :: Int -> Seq Int -> Seq Int
move top input =
  let (current :<| not_current) = input
      (picked_up, rest) = S.splitAt 3 not_current
      destination = computeDestination top current picked_up
      Just i = elemIndexL destination rest
      (front, back) = S.splitAt (i+1) rest
  in (front >< picked_up >< back) :|> current

computeDestination :: Int -> Int -> Seq Int -> Int
computeDestination top one_above picked_up =
  let target = if one_above == 1 then top else one_above - 1
  in if elem target picked_up then computeDestination top target picked_up else target

assembleAnswer :: Seq Int -> String
assembleAnswer config =
  let Just i = elemIndexL 1 config
      (front, tmp@(1 :<| rest)) = S.splitAt i config
  in concat (map show $ ((toList (rest >< front)) :: [Int]))
