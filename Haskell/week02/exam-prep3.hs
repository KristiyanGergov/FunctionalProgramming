type TvShow = (String, (Int, Int), Int)

maximumBy :: Ord b => (a -> b) -> [a] -> a
maximumBy criteria xs = foldl1 (maxBy criteria) xs
    where 
        maxBy :: Ord a => (a -> b) -> a -> a -> a
        maxBy criteria x y
            | criteria y > criteria x = y
            | otherwise = x

tvShowToMin :: TvShow -> Int
tvShowToMin (_, (hours, minutes), duration) = 
    hours * 60 + minutes + duration

lastShow :: [TvShow] -> TvShow
lastShow shows = maximumBy tvShowToMin shows

duration :: TvShow -> Int
duration (_, _, x) = x

programDuration :: [TvShow] -> Int
programDuration shows = sum (map duration shows)

subsets :: [t] -> [[t]]
subsets [] = [[]]
subsets (x:xs) = noX ++ withX
    where noX = subsets xs
        withX = map (x:) noX

longestProgram :: [TvShow] -> [TvShow]
longestProgram shows = maximumBy programDuration (candidates shows)
    where
        candidates

