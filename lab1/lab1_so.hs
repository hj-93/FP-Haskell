import Test.QuickCheck

power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)


stepsPower:: Integer -> Integer -> Integer
stepsPower n k = k + 1

power1:: Integer -> Integer -> Integer
power1 n k = product (replicate (fromInteger k) n)


power2:: Integer -> Integer -> Integer
power2 n 0 = 1
power2 n k  | even k = power (n*n) (div k 2)
power2 n k | odd k = n * power n (k-1)

prop_powers:: Integer -> Integer -> Bool
prop_powers n k = (power1 n k) == (power2 n k)  && (power1 n k) == (power n k)

powerTest :: [(Integer,Integer)]->Bool
powerTest xs = and [prop_powers x y| (x,y) <- xs]

--list for testing with even powers and odd powers.
list = [(1,0),(5,3),(5,6),(1,3)]

prop_powers' :: Integer->Integer->Bool
prop_powers' n k = prop_powers n (abs k)

