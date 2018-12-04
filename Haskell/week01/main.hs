
  factorial::Integer->Integer  
  factorial 0 = 1
  factorial n = n * factorial (n - 1)
  
  fibonacci ::Integer ->Integer
  fibonacci 0 = 0
  fibonacci 1 = 1
  fibonacci n = fibonacci (n - 2) + fibonacci (n - 1)
  
  fibonacci' :: Integer->Integer
  fibonacci' n = iter 0 1 n
    where iter a _ 0 = a
          iter a b n = iter b (a + b)(n - 1)

  
  fibs :: [Integer]
  fibs = 0 : 1 : zipWith (+) fibs (tail fibs)          
          
  fibonacci'' :: Int -> Integer
  fibonacci'' = (!!) fibs
          
  addVectors :: Num t => (t, t) -> (t, t) -> (t, t)
  addVectors a b = (fst a + fst b, snd a + snd b)

  
  compress :: Eq t => [t] -> [t]
  compress [] = []
  compress [x] = [x]
  compress (x:y:xs)
    | x == y = compress (y:xs)
    | otherwise = x : compress (y:xs)
  
  duplicate :: [t] -> [t]
  duplicate [] = []
  duplicate (x:xs) = x : x : duplicate xs
  