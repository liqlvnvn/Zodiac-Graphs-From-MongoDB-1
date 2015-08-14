{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Parsing.ParsingFromMongo where

import Parsing

import Data.Bson
import Data.Maybe (Maybe(..), fromJust)
import Data.List
import Data.Ord      ( comparing )
import Data.Tuple.Select

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f =
  map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))

--
-- First query
parse1 :: [Document] -> [(Zodiac, Int)]
parse1 = map parseDoc1
  where
    parseDoc1 :: [Field] -> (Zodiac, Int)
    parseDoc1 doc = (read (at "_id" doc) :: Zodiac, at "count" doc)

extr1 :: (Zodiac, Int) -> Zodiac
extr1 doc = fst doc

--
-- Second query
parse2 :: [Document] -> [(InfZodiac, Int)]
parse2 = map parseDoc2
  where
    parseDoc2 :: [Field] -> (InfZodiac, Int)
    parseDoc2 doc = (stringToZodiac (at "_id" doc), at "count" doc)

extr2 :: (InfZodiac, Int) -> InfZodiac
extr2 = fst

--
-- Third query
parse3 :: [Document] -> [(Zodiac, InfZodiac, Int)]
parse3 = map parseDoc3

parseDoc3 :: [Field] -> (Zodiac, InfZodiac, Int)
parseDoc3 doc = (z, z', n)
  where
    t  = at "_id" doc
    z  = read (at "zodiac" t) :: Zodiac
    z' = stringToZodiac $ at "inf_zodiac" t
    n  = at "count" doc

extr3 :: (Zodiac, InfZodiac, Int) -> Zodiac
extr3 = sel1

extr3' :: (Zodiac, InfZodiac, Int) -> InfZodiac
extr3' = sel2

--
-- Fourth query
parse4 :: [Document] -> [(Birthday, Int)]
parse4 = map parseDoc4

parseDoc4 :: [Field] -> (Birthday, Int)
parseDoc4 doc = (b, n)
  where
    b = Birthday { day = d, month = m, year = 1900 }
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
