{-| Functions for converting a response of a database
    to internal representation.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Parsing.ParsingFromMongo
  ( parse1, parse2, parse3, parse4
  ) where

import Parsing   ( Birthday(..), Zodiac, Month,
                   StatsSign, StatsInfSign,
                   StatsExactSign, StatsBirthday, stringToZodiac)
import Data.Bson ( Document, Field, at )

-- | First query
parse1 :: [Document] -> [StatsSign]
parse1 = map parseDoc1
  where
    parseDoc1 :: [Field] -> StatsSign
    parseDoc1 doc = (read (at "_id" doc) :: Zodiac, at "count" doc)

-- | Second query
parse2 :: [Document] -> [StatsInfSign]
parse2 = map parseDoc2
  where
    parseDoc2 :: [Field] -> StatsInfSign
    parseDoc2 doc = (stringToZodiac (at "_id" doc), at "count" doc)

-- | Third query
parse3 :: [Document] -> [StatsExactSign]
parse3 = map parseDoc3
  where
    parseDoc3 :: [Field] -> StatsExactSign
    parseDoc3 doc = (zodiac, infZodiac, numberOfEntries)
      where
        temp  = at "_id" doc
        zodiac  = read (at "zodiac" temp) :: Zodiac
        infZodiac = stringToZodiac $ at "inf_zodiac" temp
        numberOfEntries  = at "count" doc

-- | Fourth query
parse4 :: [Document] -> [StatsBirthday]
parse4 = map parseDoc4
  where
    parseDoc4 :: [Field] -> StatsBirthday
    parseDoc4 doc = (birthday, numberOfEntries)
      where
        numberOfEntries = at "count" doc
        birthday = Birthday { day = day', month = month', year = 1900 }
        day' = at "day" temp
        month' = read (at "month" temp) :: Month
        temp = at "_id" doc
