histogram :: [t] -> [(t, Int)]
histogram [] = []
histogram lst@(x:_) = (x, length $ filter (==x) lst) : (histogram filter (/=x) lst)

maximumBy :: Eq b => (a -> b) -> [a] -> a
maximumBy criteria xs = foldl1 (maxBy criteria) xs
    where 
        maxBy :: Eq a => (a -> b) -> a -> a -> a
        maxBy criteria x y
            | criteria y > criteria x = y
            | otherwise = x

mostFrequents :: (Eq t, Num t) => [t] -> [t]
mostFrequents xs = map fst (filter isMax hist)
    where hist = histogram xs
        (_, maxFrequency) = (x, = maximumBy snd hist
        isMax = (==maxFrequency) . snd

intersect :: [t] -> [t] -> [t]
xs `intersect` ys = filter (`elem` ys)

intersection :: Eq t => [[t]] -> [t]
intersection [] = []
intersection xs = foldl1 intersect xs

mostFrequent :: (Eq t, Num t) => [[t]] -> t
mostFrequent xs 
        | null intersect = 0
        | otherwise = head intersect
            where intersect = intersection (map mostFrequents xs)