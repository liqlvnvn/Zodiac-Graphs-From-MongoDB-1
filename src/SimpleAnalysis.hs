{-# LANGUAGE ScopedTypeVariables #-}

module SimpleAnalysis 
  ( mkAnalysisOf1stGraph
  , mkAnalysisOf2ndGraph
  , mkAnalysisOf3rdGraph
  , mkAnalysisOf4rdGraph
  ) where

import Parsing

import Data.List         ( sortBy, partition )
import Data.Tuple.Select ( sel2, sel3 )
import Data.Function     ( on )
import Data.Maybe        ( isJust, fromJust, )
import Text.Printf
import Control.Arrow     ( first )

avrgOf :: forall a s a1 a2.
                (Integral a1, Integral s, Fractional a2) =>
                [a] -> (a -> (a1, s) -> (a1, s)) -> a2
avrgOf stats func = x / y
  where
    x = fromIntegral $ snd tupleAvg
    y = fromIntegral $ fst tupleAvg
    tupleAvg = foldr func (0, 0) stats

diffFromAvrg :: forall a a1 t.
                      (Num a, Integral a1) =>
                      t -> a -> (t -> a1) -> a
diffFromAvrg stats average func = fromIntegral (func stats) - average

topAndLowest :: Ord b => [a] -> (a -> b) -> ([a], [a])
topAndLowest stats func = (take 5 sorted, take 5 $ reverse sorted)
  where
    sorted = sortBy (compare `on` func) stats

-- | First argument is result of 'differenceFromAverage',
-- second argument is result of 'averageOfSigns'
diffFromAverageInPercentages :: Float -> Float -> Float
diffFromAverageInPercentages diff average = diff / average

-- Functions for writing the results of the simple analysis
-- For first and second graph
mkAnalysisOf1stGraph :: forall a a1.
                              (Show a1, Show a, Integral a) =>
                              [(a1, a)] -> String
mkAnalysisOf1stGraph stats = 
  concat [ "Average = ", show average, "\n\n"
         , "Top5:\n"
         , showTable top, "\n\n"
         , "Top5 of the lowest\n"
         , showTable lowest, "\n"
         ]
  where
    top = snd $ topAndLowest stats snd
    lowest = fst $ topAndLowest stats snd
    average = avrgOf stats (\(_,a) (b :: Integer,c) -> (b+1,a+c))
    difference st = diffFromAvrg st average snd
    differenceIn st = diffFromAverageInPercentages (difference st) average
--      showTable :: [StatsSign] -> String
    showTable = concatMap (\x@(zodiac, number) -> concat 
                                  [ show zodiac, "\t", show number, "\t" 
                                  , printf "%10.2f" (difference x)
                                  , "\t"
                                  , printf "%5.2f" (differenceIn x)
                                  , "\n"]
                          )

-- For second
mkAnalysisOf2ndGraph :: [StatsInfSign] -> String
mkAnalysisOf2ndGraph stats = 
  concat [ mkAnalysisOf1stGraph $ map (first fromJust) (fst temp)
         , "Nothing", "\t"
         , show $ snd (head $ snd temp)
         , "\n"
         ]
  where
    -- stats -> ([(Maybe zodiac,num)],[])
    temp = partition (isJust . fst) stats
    
-- For third
mkAnalysisOf3rdGraph :: [StatsExactSign] -> String
mkAnalysisOf3rdGraph statistic = 
  concat [ "Average = ", show average, "\n\n"
         , "Top5:\n"
         , showTable top, "\n\n"
         , "Top5 of the lowest\n"
         , showTable lowest, "\n"
         ]
  where
    -- Zodiac/Nothing does not tell us much
    -- Because this category is much wider than Zodiac/Zodiac
    stats = filter (isJust . sel2) statistic
    top = snd $ topAndLowest stats sel3
    lowest = fst $ topAndLowest stats sel3
    average = avrgOf stats (\(_,_,a) (b :: Integer,c) -> (b+1,a+c))
    difference st = diffFromAvrg st average sel3
    differenceIn st = diffFromAverageInPercentages (difference st) average
--    showTable :: [StatsSign] -> String
    showTable = concatMap (\x@(zod, zod2, number) -> concat 
                                  [ show zod, "/", show $ fromJust zod2
                                  , "\t", show number, "\t" 
                                  , printf "%10.2f" (difference x)
                                  , "\t"
                                  , printf "%5.2f" (differenceIn x)
                                  , "\n"]
                          )
-- For fourth
mkAnalysisOf4rdGraph :: [StatsBirthday] -> String
mkAnalysisOf4rdGraph = mkAnalysisOf1stGraph
