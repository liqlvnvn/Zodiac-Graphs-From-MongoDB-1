module ToFile where

import Parsing.ParsingFromMongo
import Parsing

import Data.Tuple.Select (sel1, sel2, sel3)

convert1 :: StatsOnAllSigns -> String
convert1 list = foldr1 (++) $ map toString (sortOn extr1 list)
  where
    toString :: StatsOnSign -> String
    toString tpl = (show . fst) tpl ++ " " ++ (show . snd) tpl ++ "\n"

convert2 :: StatsOnAllInfSigns -> String
convert2 list = foldr1 (++) $ map toString
                                  (mvNothingToTheEnd $ sortOn extr2 list)
  where
    toString :: StatsOnInfSign -> String
    toString tpl = maybe "Nothing" show (fst tpl) ++ " "
                   ++ (show . snd) tpl ++ "\n"
    mvNothingToTheEnd :: StatsOnAllInfSigns -> StatsOnAllInfSigns
    mvNothingToTheEnd (x:xs) = xs ++ [x]
    mvNothingToTheEnd []     = []

convert3 :: StatsOnAllExactSigns -> String
convert3 list = foldr1 (++) $ map toString (switch1And2 $ sorting list)
  where
    sorting :: StatsOnAllExactSigns -> StatsOnAllExactSigns
    sorting d = sortOn extr3 (sortOn extr3' d)
    {-
    -- After sort:           Need:
    -- Aries/Nothing 15589   Aries/Taurus 6462
    -- Aries/Taurus 6462     Aries/Nothing 15589
    -- Aries/Pisces 6490     Aries/Pisces 6490
    -}
    switch1And2 :: StatsOnAllExactSigns -> StatsOnAllExactSigns
    switch1And2 (a:b:c:xs) = b:a:c:switch1And2 xs
    switch1And2 []         = []
    switch1And2 [a]        = [a]
    switch1And2 [a, b]     = [a, b]
    toString :: StatsOnExactSign -> String
    toString tpl = (show . sel1) tpl ++ "/" ++ maybe "Nothing" show (sel2 tpl)
                   ++ " " ++ (show . sel3) tpl ++ "\n"

convert4 :: StatsOnAllBirthdays -> String
convert4 list = foldr1 (++) $ map toString (sorting list)
  where
    sorting :: StatsOnAllBirthdays -> StatsOnAllBirthdays
    sorting = sortOn extr4
    toString :: StatsOnBirthday -> String
    toString tpl = (show . day . fst) tpl ++ " " ++ (show . month . fst) tpl
               ++ " " ++ (show . snd) tpl ++ "\n"
