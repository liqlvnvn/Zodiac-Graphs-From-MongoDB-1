module SimpleAnalysis where

import Parsing

import Data.List ( sortBy )
averageOf :: [(a, Int)] -> Float
averageOf = foldr1 

diffFromAverage :: (a, Int) -> Float -> Float
diffFromAverage = undefined

-- | First argument is result of 'differenceFromAverage',
--   second argument is result of 'averageOfSigns'
diffFromAverageInPercentages :: Float -> Float -> Float
diffFromAverageInPercentages = undefined

-- Analysis of the first and the second graph
top5Signs, lowest5Signs :: [(a, Int)] -> [(a, Int)]
top5Signs = undefined
lowest5Signs = undefined

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
