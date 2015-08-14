{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Parsing.ParsingFromMongo
  (
    sortOn
  , parse1, parse2, parse3, parse4
  , extr1, extr2, extr3, extr3', extr4
  ) where

import Parsing

import Data.Bson
import Data.List
import Data.Ord      ( comparing )
import Data.Tuple.Select (sel1, sel2)

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f =
  map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))

--
-- First query
parse1 :: [Document] -> StatsOnAllSigns
parse1 = map parseDoc1
  where
    parseDoc1 :: [Field] -> StatsOnSign
    parseDoc1 doc = (read (at "_id" doc) :: Zodiac, at "count" doc)

extr1 :: StatsOnSign -> Zodiac
extr1 = fst

--
-- Second query
parse2 :: [Document] -> StatsOnAllInfSigns
parse2 = map parseDoc2
  where
    parseDoc2 :: [Field] -> StatsOnInfSign
    parseDoc2 doc = (stringToZodiac (at "_id" doc), at "count" doc)

extr2 :: StatsOnInfSign -> InfZodiac
extr2 = fst

--
-- Third query
parse3 :: [Document] -> StatsOnAllExactSigns
parse3 = map parseDoc3

parseDoc3 :: [Field] -> StatsOnExactSign
parseDoc3 doc = (z, z', n)
  where
    t  = at "_id" doc
    z  = read (at "zodiac" t) :: Zodiac
    z' = stringToZodiac $ at "inf_zodiac" t
    n  = at "count" doc

extr3 :: StatsOnExactSign -> Zodiac
extr3 = sel1

extr3' :: StatsOnExactSign -> InfZodiac
extr3' = sel2

--
-- Fourth query
parse4 :: [Document] -> StatsOnAllBirthdays
parse4 = map parseDoc4

parseDoc4 :: [Field] -> StatsOnBirthday
parseDoc4 doc = (b, n)
  where
    b = Birthday { day = d, month = m, year = 1900 }
    n = at "count" doc
    t = at "_id" doc
    d = at "day" t
    m = read (at "month" t) :: Month

extr4 :: StatsOnBirthday -> Birthday
extr4 = fst
