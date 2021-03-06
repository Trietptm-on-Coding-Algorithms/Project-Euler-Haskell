import Data.List
import Data.Char

divisors :: Integer -> [Integer]
divisors 1 = [1]
divisors x = (nub . sort . flatten) [(k, x `div` k) | k <- [1..floor (sqrt (fromInteger x))], x `mod` k == 0]
    where flatten [] = []
          flatten ((x, y):ys) = x : y : flatten ys

collatzchain :: Integer -> [Integer]
collatzchain 1 = [1]
collatzchain n = n : if odd n
                        then collatzchain (3 * n + 1)
                        else collatzchain (n `div` 2)

fibo :: Int -> Integer
fibo 1 = 1
fibo k =
    let helper 0 (x,y) = y
        helper 1 (x,y) = x
        helper n (x,y) = helper (n - 1) (x + y, x)
    in helper k (1,0)

p15 = product [21..40] `div` product [1..20]

-- 9^5 = 59049 -- 9
-- 9^5 * 6 = 354294 -- 99999
-- 9^5 * 7 = 413343 -- 999999
p30 :: Int
p30 = sum . filter (\x -> (sum . map ((^5) . digitToInt) . show) x == x) $ [10..413343]

-- 9! = 362880
-- 9! * 7 = 2540160
-- 9! * 8 = 2903040
p34 :: Int
p34 = sum . filter (\x -> (sum . map ((\x -> product [1..x]) . digitToInt) . show) x == x) $ [10..2540160]

p48 :: Integer
p48 = sum [a^a | a <- [1..1000]] `mod` 10 ^ 10
