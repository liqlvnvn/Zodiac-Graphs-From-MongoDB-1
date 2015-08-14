module ToFile where

import Parsing.ParsingFromMongo
import Parsing

import Data.Tuple.Select

convert1 :: [(Zodiac, Int)] -> String
convert1 list = foldr1 (++) $ map toString (sortOn extr1 list)
  where
    toString :: (Zodiac, Int) -> String
    toString tpl = (show . fst) tpl ++ " " ++ (show . snd) tpl ++ "\n"

convert2 :: [(InfZodiac, Int)] -> String
convert2 list = foldr1 (++) $ map toString
                                  (mvNothingToTheEnd $ sortOn extr2 list)
  where
    toString :: (InfZodiac, Int) -> String
    toString tpl = maybe "Nothing" show (fst tpl) ++ " "
                   ++ (show . snd) tpl ++ "\n"
    mvNothingToTheEnd :: [(InfZodiac, Int)] -> [(InfZodiac, Int)]
    mvNothingToTheEnd (x:xs) = xs ++ [x]
    mvNothingToTheEnd []     = []

convert3 :: [(Zodiac, InfZodiac, Int)] -> String
convert3 list = foldr1 (++) $ map toString (switch2And3 $ sorting list)
  where
    sorting :: [(Zodiac, InfZodiac, Int)] -> [(Zodiac, InfZodiac, Int)]
    sorting d = sortOn extr3 (sortOn extr3' d)
    {-
    -- After sort:           Need:
    -- Aries/Taurus 6462     Aries/Taurus 6462
    -- Aries/Nothing 15589   Aries/Pisces 6490
    -- Aries/Nothing 15589   Aries/Pisces 6490
    -}
    switch2And3 :: [(Zodiac, InfZodiac, Int)] -> [(Zodiac, InfZodiac, Int)]
    switch2And3 (a:b:c:xs) = a:c:b:switch2And3 xs
    switch2And3 []         = []
    switch2And3 [a]        = [a]
    switch2And3 [a, b]     = [a, b]
    toString :: (Zodiac, InfZodiac, Int) -> String
    toString tpl = (show . sel1) tpl ++ "/" ++ maybe "Nothing" show (sel2 tpl) ++ " "
               ++ (show . sel3) tpl ++ "\n"

convert4 :: [(Birthday, Int)] -> String
convert4 list = foldr1 (++) $ map toString (sorting list)
  where
    sorting :: [(Birthday, Int)] -> [(Birthday, Int)]
    sorting d = sortOn extr4 d
    toString :: (Birthday, Int) -> String
    toString tpl = (show . day . fst) tpl ++ " " ++ (show . month . fst) tpl
               ++ " " ++ (show . snd) tpl ++ "\n"
