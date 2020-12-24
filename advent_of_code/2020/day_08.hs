import Data.List
import qualified Data.Set as S
import qualified Data.Vector as V
import System.Environment

main = do
  args <- getArgs
  content <- readFile (args !! 0)

  let insts = V.fromList (map (parseInst . words) (lines content))
      init_state =  (State S.empty 0 0)
      num_insts = length insts

  let basic_endstate = runProgram (BP insts) init_state
    in putStrLn $ "Base Program, accumulator at start of repeat: " ++ (show (getAcc basic_endstate))

  let overlays = map ((\p -> runProgram p init_state) . (OP insts)) [0..(num_insts - 1)]
      good_overlay = find (\o -> (getPtr o) == num_insts) overlays
    in case good_overlay of
         Just s -> putStrLn $ "Accumulator after overlay end: " ++ (show (getAcc s))
         Nothing -> putStrLn $ "Good overlay not found."
    
data Inst = Acc Int | Jmp Int | Nop Int deriving (Show)
data State = State (S.Set Int) Int Int deriving (Show)

getAcc :: State -> Int
getAcc (State _ _ a) = a

getPtr :: State -> Int
getPtr (State _ p _) = p

parseInst :: [String] -> Inst
parseInst (i:ns:[]) =
  case i of
    "acc" -> Acc ni
    "jmp" -> Jmp ni
    "nop" -> Nop ni
  where ni = read (dropWhile (== '+') ns)
parseInst _ = error "Instruction with more than two words."

class Puter p where
  size :: p -> Int
  getInst :: p -> Int -> Inst

data BasicPuter = BP (V.Vector Inst)
instance Puter BasicPuter where
  size (BP v) = length v
  getInst (BP v) i = v V.! i

data OverlayPuter = OP (V.Vector Inst) Int
instance Puter OverlayPuter where
  size (OP v o) = length v
  getInst (OP v o) i =
    let original_inst = v V.! i
    in if (i /= o)
       then original_inst
       else 
         case original_inst of
              Acc n -> Acc n
              Jmp n -> Nop n
              Nop n -> Jmp n

runProgram :: (Puter p) => p -> State -> State
runProgram puter state@(State found ptr acc) =
  if S.member ptr found || ptr == (size puter)
  then state
  else let newfound = S.insert ptr found
       in case (getInst puter ptr) of
            Acc n -> runProgram puter (State newfound (ptr + 1) (acc + n))
            Jmp n -> runProgram puter (State newfound (ptr + n) acc)
            Nop _ -> runProgram puter (State newfound (ptr + 1) acc)
