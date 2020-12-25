import Data.List
import Data.Maybe
import System.Environment

main = do
  let modulus = 20201227
      subject = 7
      card_key = 3248366
      door_key = 4738476
      
      card_loop_size = findLoopSize card_key subject modulus

  putStrLn $ "Encryption key = " ++ (show ((powerList subject modulus) !! card_loop_size))

nextPower :: Int -> Int -> Int -> Int
nextPower subject modulus prev = rem (prev * subject) modulus

powerList :: Int -> Int -> [Int]
powerList subject modulus = iterate (nextPower subject modulus) 1

findLoopSize :: Int -> Int -> Int -> Int
findLoopSize key subject modulus =
  (snd . fromJust) $ find ((== key) . fst) (zip (powerList subject modulus) [0..])
