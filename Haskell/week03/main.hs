-- T A S K 1
pythagoreanTriples :: Integral t => [(t, t, t)]
pythagoreanTriples = [(a, b, c) | c <- [1..998], 
                                  b <- [1..(999 - c)], 
                                  let a = 1000 - b - c,
                                  a <= b,
                                  a^2 + b^2 == c^2]
-- T A S K 1         

-- T A S K 2                                  
divides :: Integral t => t -> t -> Bool
divisor `divides` dividend = dividend `mod` divisor == 0

divisibleByEvery :: Integral t => t -> [t] -> Bool
dividend `divisibleByEvery` divisors = all(`divides` dividend) divisors
                                  
smallestMultiple :: Integral t => t -> t
smallestMultiple n = head [x | x <- [n..], x `divisibleByEvery` [1..n]]
-- T A S K 2

-- T A S K 3
prime :: Integral t => t -> Bool
prime n = all (not . (`divides` n)) [2..n-1] 

sumPrimesLowerThan :: Int -> Int

--S I M P L E   S O L U T I O N
--sumPrimesLowerThan n = sum [x | x <- [2..n], x < n, prime x]

primes :: Integral t => [t]
primes = sieve [2..]
  where sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x > 0]
  
sumPrimesLowerThan n = sum (takeWhile (<n) primes)
-- T A S K 3