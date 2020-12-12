import Data.List
import System.Environment

main = do
  args <- getArgs
  content <- readFile (args !! 0)

  let insts = (lines content)
      init_state = St (0, 0) 'E'
      end_state@(St p _)  = foldl' updateState init_state insts

  putStrLn $ "End state: " ++ (show end_state)
  putStrLn $ "Manhattan Distance: " ++ (show (manhattan p))

  let init_wst = WSt (0, 0) (10, 1)
      end_wst@(WSt wp _) = foldl' updateWSt init_wst insts

  putStrLn $ "End waypoint state: " ++ (show end_wst)
  putStrLn $ "Waypoint Manhattan Distance: " ++ (show (manhattan wp))

data State = St (Int, Int) Char deriving (Show)

manhattan :: (Int, Int) -> Int
manhattan p@(x, y) = ((abs x) + (abs y))


nextDir :: Char -> Char
nextDir 'N' = 'E'
nextDir 'E' = 'S'
nextDir 'S' = 'W'
nextDir 'W' = 'N'

updateState :: State -> String -> State
updateState state@(St (x, y) dir) inst@(ch:numstr) 
  | ch == 'N' = St (x, y + num) dir
  | ch == 'E' = St (x + num, y) dir
  | ch == 'S' = St (x, y - num) dir
  | ch == 'W' = St (x - num, y) dir
  | ch == 'L' = updateState state ('R':(show (360 - num)))
  | ch == 'R' = if num == 0 then state else updateState (St (x, y) (nextDir dir)) ('R':(show (num - 90)))
  | ch == 'F' = updateState state (dir:numstr)
  where num = (read numstr) :: Int
                                                                                     
data WSt = WSt (Int, Int) (Int, Int) deriving (Show)

updateWSt :: WSt -> String -> WSt
updateWSt wst@(WSt (x, y) (wx, wy)) inst@(ch:numstr)
  | ch == 'N' = WSt (x, y) (wx, wy + num)  
  | ch == 'E' = WSt (x, y) (wx + num, wy)  
  | ch == 'S' = WSt (x, y) (wx, wy - num)  
  | ch == 'W' = WSt (x, y) (wx - num, wy)  
  | ch == 'L' = updateWSt wst ('R':(show (360 - num)))
  | ch == 'R' = if num == 0 then wst else updateWSt (WSt (x, y) (wy, -wx)) ('R':(show (num - 90)))
  | ch == 'F' = WSt (x + num*wx, y + num * wy) (wx, wy)
  where num = (read numstr) :: Int

