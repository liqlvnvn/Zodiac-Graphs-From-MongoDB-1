{-| Functions for conversion statistics from the internal representation
    of the program (see Parsing.hs to learn data types) in lines
    for output files.
-}

module ToFile
  (
    convert1
  , convert2
  , convert3
  , convert4
  ) where

import Parsing (  Birthday(..), Zodiac, InfZodiac, StatsSign,
                  StatsInfSign, StatsExactSign, StatsBirthday,
                  StatsAllSigns, StatsAllInfSigns, StatsAllExactSigns,
                  StatsAllBirthdays )

import Data.Tuple.Select ( sel1, sel2, sel3 )
import Data.Ord ( comparing )
import Data.List ( sortBy )

-- | query1
convert1 :: StatsAllSigns -> String
convert1 list = foldr1 (++) $ map toString (sortOn extr1 list)
  where
    toString :: StatsSign -> String
    toString tpl = (show . fst) tpl ++ " " ++ (show . snd) tpl ++ "\n"

-- | query2
convert2 :: StatsAllInfSigns -> String
convert2 list = foldr1 (++) $ map toString
                                  (mvNothingToTheEnd $ sortOn extr2 list)
  where
    toString :: StatsInfSign -> String
    toString tpl = maybe "Nothing" show (fst tpl) ++ " "
                   ++ (show . snd) tpl ++ "\n"
    mvNothingToTheEnd :: StatsAllInfSigns -> StatsAllInfSigns
    mvNothingToTheEnd (x:xs) = xs ++ [x]
    mvNothingToTheEnd []     = []

-- | query3
convert3 :: StatsAllExactSigns -> String
convert3 list = foldr1 (++) $ map toString (switch1And2 $ sorting list)
  where
    sorting :: StatsAllExactSigns -> StatsAllExactSigns
    sorting d = sortOn extr3 (sortOn extr3' d)
    {-
    -- After sort:           Need:
    -- Aries/Nothing 15589   Aries/Taurus 6462
    -- Aries/Taurus 6462     Aries/Nothing 15589
    -- Aries/Pisces 6490     Aries/Pisces 6490
    -}
    switch1And2 :: StatsAllExactSigns -> StatsAllExactSigns
    switch1And2 (a:b:c:xs) = b:a:c:switch1And2 xs
    switch1And2 []         = []
    switch1And2 [a]        = [a]
    switch1And2 [a, b]     = [a, b]
    toString :: StatsExactSign -> String
    toString tpl = (show . sel1) tpl ++ "/"
                   ++ maybe "Nothing" show (sel2 tpl)
                   ++ " " ++ (show . sel3) tpl ++ "\n"

-- | query4
convert4 :: StatsAllBirthdays -> String
convert4 list = foldr1 (++) $ map toString (sorting list)
  where
    sorting :: StatsAllBirthdays -> StatsAllBirthdays
    sorting = sortOn extr4
    toString :: StatsBirthday -> String
    toString tpl = (show . day . fst) tpl ++ " "
                   ++ (show . month . fst) tpl
                   ++ " " ++ (show . snd) tpl ++ "\n"

-- | Helper functions for sorting before output.
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f =
  map snd . sortBy (comparing fst) . map (\x -> let y = f x
                                                in y `seq` (y, x))

extr1 :: StatsSign -> Zodiac
extr1 = fst

extr2 :: StatsInfSign -> InfZodiac
extr2 = fst

extr3 :: StatsExactSign -> Zodiac
extr3 = sel1

extr3' :: StatsExactSign -> InfZodiac
extr3' = sel2

extr4 :: StatsBirthday -> Birthday
extr4 = fst
