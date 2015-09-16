module SimpleAnalysis where

import Parsing

import Data.List ( sortBy )

-- Проблема в типе а функция из ToFile не может show (a0)
averageOf :: [(a, Int)] -> Float
averageOf stats = x / y
  where
    x = fromIntegral $ snd tupleAvg
    y = fromIntegral $ fst tupleAvg
    tupleAvg = foldr (\(_,a) (b,c) -> (b+1,a+c)) (0, 0) stats

diffFromAverage :: (a, Int) -> Float -> Float
diffFromAverage stats average = (fromIntegral $ snd stats) - average

-- | First argument is result of 'differenceFromAverage',
-- second argument is result of 'averageOfSigns'
diffFromAverageInPercentages :: Float -> Float -> Float
diffFromAverageInPercentages diff average = diff / average

-- Analysis of the first and the second graph
top5Signs, lowest5Signs :: [(a, Int)] -> [(a, Int)]
top5Signs stats = take 5 (sortBy (comp) stats )
  where
    comp x y = compare (snd x) (snd y)
lowest5Signs = undefined
--lowest5Signs stats = take 5 (sortBy (compare . snd) stats )


-- Analysis of the third
top5By1and2Signs, lowest5By1and2Signs :: StatsAllExactSigns
                                      -> [StatsExactSign]
top5By1and2Signs = undefined
lowest5By1and2Signs = undefined

averageOfFullSigns :: StatsAllExactSigns -> Float
averageOfFullSigns = undefined

diffFromAverageForFullSigns :: StatsAllExactSigns -> Float -> Float
diffFromAverageForFullSigns = undefined

-- Analysis of the fourth
top5Days, lowest5Days :: StatsAllBirthdays -> [StatsBirthday]
top5Days = undefined
lowest5Days = undefined
