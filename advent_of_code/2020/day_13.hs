import Data.List
import Data.List.Split
import Data.Ord
import System.Environment

main = do
  args <- getArgs
  content <- readFile (args !! 0)

  let (timestamp_s:(bus_ids_s:[])) = lines content
      timestamp = read timestamp_s :: Int
      all_bus_ids_s = (splitOn "," bus_ids_s)
      ids_and_rs = map (\(i, r) -> (read i, r))  $ filter (((/=) "x") . fst) (zip all_bus_ids_s [0..])
      bus_ids = map fst ids_and_rs
      bus_wait_time bus_id = (bus_id - (timestamp `mod` bus_id)) `mod` bus_id
      first_bus = minimumBy (comparing fst) (zip (map bus_wait_time bus_ids) bus_ids)
      -- mod_ids_and_rs = map (\(i, r) -> (toInteger i, toInteger ((i - (r `mod` i)) `mod` i))) ids_and_rs
      mod_ids_and_rs = map (\(i, r) -> (toInteger i, toInteger ((-r) `mod` i))) ids_and_rs
      new_timestamp = foldr1 crt2 mod_ids_and_rs
  
  putStrLn $ "Bus times: " ++ (show first_bus) ++ ", Product: " ++ (show ((uncurry (*)) first_bus))
  putStrLn $ "Timestamp = " ++ (show new_timestamp)
  putStrLn $ (show ids_and_rs)
  putStrLn $ (show mod_ids_and_rs)

extended_euclid :: Integral a => a -> a -> (a, a, a)
extended_euclid x y
  | x > y     = extended_euclid' (x, 1, 0) (y, 0, 1)
  | otherwise = (r, t, s) where (r, s, t) = extended_euclid' (y, 1, 0) (x, 0, 1)
                             
extended_euclid' :: Integral a => (a, a, a) -> (a, a, a) -> (a, a, a)
extended_euclid' i0@(r0, s0, t0) i1@(r1, s1, t1) =
  if r1 == 0
  then i0
  else extended_euclid' i1 (r0 - q*r1, s0 - q*s1, t0 - q*t1)
  where q = r0 `div` r1

crt2 :: Integral a => (a, a) -> (a, a) -> (a, a)
crt2 (n1, a1) (n2, a2) = (prod, res)
  where (_, m1, m2) = extended_euclid n1 n2
        prod = n1*n2
        res = (a1*m2*n2 + a2*m1*n1) `mod` prod
        
