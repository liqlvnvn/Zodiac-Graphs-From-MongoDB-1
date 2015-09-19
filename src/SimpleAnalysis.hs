module SimpleAnalysis where

import Parsing

import Data.List         ( sortBy )
import Data.Tuple.Select ( sel3 )

-- Проблема в типе а функция из ToFile не может show (a0)
--averageOf :: [(a, Int)] -> Float
averageOf stats = x / y
  where
    x = fromIntegral $ snd tupleAvg
    y = fromIntegral $ fst tupleAvg
    tupleAvg = foldr (\(_,a) (b,c) -> (b+1,a+c)) (0, 0) stats

--diffFromAverage :: (a, Int) -> Float -> Float
diffFromAverage stats average = (fromIntegral $ snd stats) - average

-- | First argument is result of 'differenceFromAverage',
-- second argument is result of 'averageOfSigns'
diffFromAverageInPercentages :: Float -> Float -> Float
diffFromAverageInPercentages diff average = diff / average

-- Analysis of the first and the second graph
--top5Signs, lowest5Signs :: [(a, Int)] -> [(a, Int)]
top5AndLowestSigns stats = (take 5 sorted, take 5 $ reverse sorted)
  where
    sorted = sortBy (comp) stats
    comp x y = compare (snd x) (snd y)
--lowest5Signs stats = take 5 (sortBy (compare . snd) stats )


-- Analysis of the third
top5AndLowestFullSigns :: [StatsExactSign] 
                       -> ([StatsExactSign], [StatsExactSign])
top5AndLowestFullSigns = undefined

averageOfFullSigns :: [StatsExactSign] -> Float
averageOfFullSigns stats = x / y
  where
    x = fromIntegral $ snd tupleAvg
    y = fromIntegral $ fst tupleAvg
    tupleAvg = foldr (\(_,_,a) (b,c) -> (b+1,a+c)) (0,0) stats

diffFromAverageForFullSigns :: [StatsExactSign] -> Float -> Float
diffFromAverageForFullSigns = undefined

-- Analysis of the fourth
top5Days, lowest5Days :: [StatsBirthday] -> [StatsBirthday]
top5Days = undefined
lowest5Days = undefined
