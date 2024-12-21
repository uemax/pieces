-- | Generating Primes (the sieve of Eratosthenes)
-- |
-- | To generate the infinite sequence of primes:
-- |
-- |   1. write down the infinite sequence 2, 3, 4...
-- |   2. mark the first number p as being prime
-- |   3. delete all multiples of p from the sequence
-- |   4. return to the second step

primes = sieve [2..]

-- | >>>   sieve [2..]
-- | >>> = 2 : sieve [3..]
-- | >>> = 2 : 3 : sieve [5..]
-- | >>> = 2 : 3 : 5 : sieve [7..]
-- | >>> ...
sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve [x | x <- xs, mod x p /= 0]

twin :: (Int, Int) -> Bool
twin (x, y) = y == x+2

twins :: [(Int, Int)]
twins = filter twin (zip primes (tail primes))

main :: IO ()
main = do print (take 10 primes)
          print (take 10 twins)
