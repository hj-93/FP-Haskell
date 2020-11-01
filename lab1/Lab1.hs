import Test.QuickCheck

{- Lab 1
   Date: 
   Authors:
   Lab group:
 -}
--------------------------------------------
power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)

-- A ------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute
-- Answer:
stepsPower :: Integer -> Integer -> Integer
stepsPower n k = k + 1

-- B -------------------------
-- power1
-- Answer:
power1 :: Integer -> Integer -> Integer
power1 n k
  |k < 0  = error "power: negative argument"
  |k == 0 = 1
  |k > 0  = product(replicate (fromInteger k) (fromInteger n))

-- C -------------------------
-- power2
-- Answer:
power2 :: Integer -> Integer -> Integer
power2 n k
  |k < 0      = error "power: negative argument"
  |even k     = power1 (n*n) (k `div` 2)
  |otherwise  = n * (power1 (n*n) ((k-1) `div` 2))

-- D -------------------------
{- 
  Considering the restrictions on k, it is valid to make testcases which
  test scenario where k<0, k=0, k>0 and odd, k>0 and even.

  Also 0 power of 0 is a valid and special scenario which shall produce 1.

  Large k is worth a test as well.

  In summary, 6 Scanerios are considered good to have:
  Testing Scenarios    n    k
   1. k < 0            5    -1
   2. k = 0            5    0
   3. k > 0 and odd    5    5
   4. k > 0 and even   5    6
   5. k >> 0           5    1000
   6. n = 0, k = 0     0    0
 -}

--
-- Answer:
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = (power n k  == power1 n k) &&
                  (power1 n k == power2 n k)

--
-- Answer:
nList :: [Integer]
kList :: [Integer]
nList = [5, 5, 5, 5, 0]
kList = [0, 5, 6, 1000, 0]

powerTest :: [Integer] ->  [Integer] -> Bool
powerTest [n] [k] = prop_powers n k
powerTest (n:ns) (k:ks) = prop_powers n k && powerTest ns ks

--
-- Answer:
prop_powers' n k
  | k < 0      = prop_powers' n (abs k)
  | otherwise  = power n k  == power1 n k &&
                 power1 n k == power2 n k


