{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Parsing.ParsingFromMongo where

import Parsing

import Data.Bson
import Data.Maybe
import Data.List
import Data.Ord      ( comparing )
import Data.Tuple.Select

parse1 :: [Document] -> [(String, Int)]
parse1 = map parseDoc1
    where parseDoc1 :: [Field] -> (String, Int)
          parseDoc1 doc = (at "_id" doc, at "count" doc)

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f =
    map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))

extr1 :: (String, Int) -> Zodiac
extr1 doc = fromJust $ stringToZodiac $ fst doc

extr2 :: (String, Int) -> Int
extr2 doc = fromJust $ elemIndex (fst doc) zodiacSigns

--
-- Third graph

parse2 :: [Document] -> [(String, String, Int)]
parse2 = map parseDoc2

parseDoc2 :: [Field] -> (String, String, Int)
parseDoc2 doc = (z, z', n)
    where t  = at "_id" doc
          z  = at "zodiac" t
          z' = at "inf_zodiac" t
          n  = at "count" doc

extr3 :: (String, String, Int) -> Int
extr3 doc = fromJust $ elemIndex (sel1 doc) zodiacSigns

extr3' :: (String, String, Int) -> Int
extr3' doc = fromJust $ elemIndex (sel2 doc) zodiacSigns

append3 :: [a] -> [a] -> [a] -> [a]
append3 a b c = a ++ b ++ c

--
-- Third graph

parse3 :: [Document] -> [(Birthday, Int)]
parse3 = map parseDoc3

parseDoc3 :: [Field] -> (Birthday, Int)
parseDoc3 doc = (b, n)
    where b = Birthday { day = d, month = m, year = 1900 }
          n = at "count" doc
          t = at "_id" doc
          d = at "day" t
          m = getMonth $ at "month" t

extr4 :: (Birthday, Int) -> Birthday
extr4 doc = fst doc

stringToZodiac :: String -> Maybe Zodiac
stringToZodiac zod = case zod of
    "Aries"       -> Just Aries
    "Taurus"      -> Just Taurus
    "Gemini"      -> Just Gemini
    "Cancer"      -> Just Cancer
    "Leo"         -> Just Leo
    "Virgo"       -> Just Virgo
    "Libra"       -> Just Libra
    "Scorpius"    -> Just Scorpius
    "Sagittarius" -> Just Sagittarius
    "Capricorn"   -> Just Capricorn
    "Aquarius"    -> Just Aquarius
    "Pisces"      -> Just Pisces
    "Nothing"     -> Nothing
    _             -> error "Wrong zodiac sign!"

getMonth :: String -> Month
getMonth mon = case mon of
    "January"   -> January
    "February"  -> February
    "March"     -> March
    "April"     -> April
    "May"       -> May
    "June"      -> June
    "July"      -> July
    "August"    -> August
    "September" -> September
    "October"   -> October
    "November"  -> November
    "December"  -> December
    _           -> error "Wrong month"
