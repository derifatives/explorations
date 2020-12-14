import Data.List
import qualified Data.Map.Strict as M
import System.Environment

main = do
  args <- getArgs
  content <- readFile (args !! 0)

  let init_state = (St (take 36 (repeat 'X')) M.empty)
      word_lines = map words (lines content)
      final_state@(St _ mem) = foldl' (processLine processMemLine) init_state word_lines
  putStrLn $ (show (foldr (+) 0 mem))

  let final_state_2@(St _ mem2) = foldl' (processLine processMemLine2) init_state word_lines
  putStrLn $ (show (foldr (+) 0 mem2))

data State = St String (M.Map Integer Integer) deriving (Show)

processLine ::  (State -> [String] -> State) -> State -> [String] -> State
processLine processMemLineFn st@(St mask mem) line
  | line !! 0 == "mask" = (St (reverse (line !! 2)) mem)  -- LSB first.
  | otherwise           = processMemLineFn st line

processMemLine :: State -> [String] -> State
processMemLine st@(St mask mem) line =
  let (loc, input) = parseMemLine line
      val = getMaskedValue input mask
  in St mask (M.insert loc val mem)

parseMemLine :: [String] -> (Integer, Integer)
parseMemLine line =  ((read . init . (drop 4)) (line !! 0),
                      (read (line !! 2) :: Integer))

getMaskedValue :: Integer -> String -> Integer
getMaskedValue input mask =
  fromBits $ map (uncurry maskedBitVal) (zip ((toBits input) ++ (repeat 0)) mask)

maskedBitVal :: Int -> Char -> Int
maskedBitVal _ '1' = 1
maskedBitVal _ '0' = 0
maskedBitVal i _   = i
                     
-- Least significant bit first.
toBits :: Integer -> [Int]
toBits 0 = []
toBits i = let (q, r) = quotRem i 2 in (fromIntegral r :: Int):(toBits q)

fromBits :: [Int] -> Integer
fromBits = foldl' (\t b -> (2 :: Integer) * t + (toInteger b)) (0 :: Integer) . reverse

processMemLine2 :: State -> [String] -> State
processMemLine2 st@(St mask mem) line =
  let (loc, val) = parseMemLine line
      memlocs = (map fromBits (floatMaskToBits (maskMemory mask loc)))
  in St mask (foldl (\m l -> M.insert l val m) mem memlocs)

maskMemory :: String -> Integer -> String
maskMemory mask memloc =
  map (uncurry maskMemoryBit) (zip mask ((toBits memloc) ++ (repeat 0)))

maskMemoryBit :: Char -> Int-> Char
maskMemoryBit 'X' _ = 'X'
maskMemoryBit '0' 0 = '0'
maskMemoryBit '0' 1 = '1'
maskMemoryBit '1' _ = '1'

floatMaskToBits :: String -> [[Int]]
floatMaskToBits [] = [[]]
floatMaskToBits ('X':bs) =
  concat([map (0:) rest, map (1:) rest]) where rest = floatMaskToBits bs
floatMaskToBits (c:bs) = map ((read [c]):) (floatMaskToBits bs)

