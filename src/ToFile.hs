{-| Functions for conversion statistics from the internal representation
    of the program (see Parsing.hs to learn data types) in lines
    for output files.
-}

module ToFile
  ( convert1
  , convert2
  , convert3
  , convert4
  , mkAnalysisOfGraph1
  , mkAnalysisOf2ndGraph
  , mkAnalysisOf3rdGraph
  ) where

import Parsing ( Birthday(..), StatsSign, 
                 StatsInfSign, StatsExactSign, StatsBirthday,
               )
import SimpleAnalysis

import Data.Tuple.Select ( sel1, sel2, sel3 )
import Data.Ord          ( comparing )
import Data.List         ( sortBy, groupBy, partition )
import Text.Printf
import Data.Maybe        ( fromJust, fromMaybe )

-- | query1
convert1 :: [StatsSign] -> String
convert1 list = foldr1 (++) $ map toString (sortOn fst list)
  where
    toString tpl = (show . fst) tpl ++ "\t" ++ (show . snd) tpl ++ "\n"

-- | query2
convert2 :: [StatsInfSign] -> String
convert2 list = foldr1 (++) $ map toString
                                  (mvNothingToTheEnd $ sortOn fst list)
  where
    toString :: StatsInfSign -> String
    toString tpl = maybe "Nothing" show (fst tpl) ++ " "
                   ++ (show . snd) tpl ++ "\n"
    mvNothingToTheEnd :: [StatsInfSign] -> [StatsInfSign]
    mvNothingToTheEnd (x:xs) = xs ++ [x]
    mvNothingToTheEnd []     = []

-- | query3
convert3 :: [StatsExactSign] -> String
--convert3 list = foldr1 (++) $ map toString (switch1And2 $ sorting list)
convert3 list = concat $ toString (switch1And2 $ sorting list)
  where
    sorting :: [StatsExactSign] -> [StatsExactSign]
    sorting d = sortOn sel1 (sortOn sel2 d)
    {-
    -- After sort:           Need:
    -- Aries/Nothing 15589   Aries/Taurus 6462
    -- Aries/Taurus 6462     Aries/Nothing 15589
    -- Aries/Pisces 6490     Aries/Pisces 6490
    -}
    switch1And2 :: [StatsExactSign] -> [StatsExactSign]
    switch1And2 (a:b:c:xs) = b:a:c:switch1And2 xs
    switch1And2 []         = []
    switch1And2 [a]        = [a]
    switch1And2 [a, b]     = [a, b]
    toString :: [StatsExactSign] -> [String]
    toString tpl = [v | a:b:c:[] <- groupBy (\x y -> (sel1 x) == (sel1 y)) tpl,
                        let x = show $ sel1 a,
                        let y = show $ sel3 a,
                        let z = show $ sel3 b,
                        let s = show $ sel3 c,
                        let v = concat [x, " ", y, " ", z," ", s,"\n"]]
{-
    This is version look like
    Aries/Nothing 15589   Aries/Taurus 6462
    Aries/Taurus 6462     Aries/Nothing 15589
    Aries/Pisces 6490

    toString tpl = (show . sel1) tpl ++ "/"
                   ++ maybe "Nothing" show (sel2 tpl)
                     ++ " " ++ (show . sel3) tpl ++ "\n"
-}

-- | query4
convert4 :: [StatsBirthday] -> String
convert4 list = foldr1 (++) $ map toString (sortOn fst list)
  where
    toString :: StatsBirthday -> String
    toString tpl = (show . day . fst) tpl ++ " "
                   ++ (show . month . fst) tpl
                   ++ " " ++ (show . snd) tpl ++ "\n"

-- | Helper functions for sorting before output.
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f =
  map snd . sortBy (comparing fst) . map (\x -> let y = f x
                                                in y `seq` (y, x))

-- Functions for writing the results of the simple analysis
-- For first and second graph
--mkAnalysisOfGraph1 :: [StatsSign] -> String
mkAnalysisOfGraph1 stats = 
  concat [ "Average = ", show $ average, "\n\n"
         , "Top5:\n"
         , showTable $ snd $ top5AndLowestSigns stats, "\n\n"
         , "Top5 of the lowest\n"
         , showTable $ fst $ top5AndLowestSigns stats, "\n"
         ]
    where
      average = averageOf stats
      difference st = diffFromAverage st average
      differenceIn st = diffFromAverageInPercentages (difference st) average
--      showTable :: [StatsSign] -> String
      showTable st = concat $ map (\x@(zodiac, number) -> concat 
                                    [ show zodiac, "\t", show number, "\t" 
                                    , printf "%10.2f" (difference x)
                                    , "\t"
                                    , printf "%5.2f" (differenceIn x)
                                    , "\n"]
                                  ) st

-- For second
mkAnalysisOf2ndGraph :: [StatsInfSign] -> String
mkAnalysisOf2ndGraph stats = concat [ mkAnalysisOfGraph1 $ map (\(x,y) ->
                                        (fromJust x,y)) $ fst temp
                                    , "Nothing", "\t"
                                    , show $ snd ((snd temp) !! 0)
                                    , "\n"
                                    ]
  where
    -- stats -> ([(Maybe zodiac,num)],[])
    temp = partition (\(x,_) -> x /= Nothing) stats
    
-- For third
mkAnalysisOf3rdGraph :: [StatsExactSign] -> String
mkAnalysisOf3rdGraph statistic = 
  concat [ "Average = ", show $ average, "\n\n"
         , "Top5:\n"
         , showTable $ snd $ top5AndLowestFullSigns stats, "\n\n"
         , "Top5 of the lowest\n"
         , showTable $ fst $ top5AndLowestFullSigns stats, "\n"
         ]
    where
      -- Zodiac/Nothing does not tell us much
      -- Because this category is much wider than Zodiac/Zodiac
      stats = filter (\(_,a,_) -> a /= Nothing) statistic
      average = averageOfFullSigns stats
      difference st = diffFromAverageForFullSigns st average
      differenceIn st = diffFromAverageInPercentages (difference st) average
--      showTable :: [StatsSign] -> String
      showTable st = concat $ map (\x@(zod, zod2, number) -> concat 
                                    [ show zod, "/", show $ fromJust zod2
                                    , "\t", show number, "\t" 
                                    , printf "%10.2f" (difference x)
                                    , "\t"
                                    , printf "%5.2f" (differenceIn x)
                                    , "\n"]
                                  ) st
-- For fourth
mkAnalysisOf4rdGraph :: [StatsBirthday] -> String
mkAnalysisOf4rdGraph = undefined
