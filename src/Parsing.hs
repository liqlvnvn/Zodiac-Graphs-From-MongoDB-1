{-# LANGUAGE FlexibleInstances #-}

module Parsing where

import qualified Text.Read.Lex as L
import GHC.Read
--import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec

data Person = Person
  { firstName :: String
  , lastName  :: String
  } deriving Show

data Birthday = Birthday
  { day   :: Int
  , month :: Month
  , year  :: Int
  } deriving Show

instance Eq Birthday where
  (==) Birthday {day = d1, month = m1, year = y1}
       Birthday {day = d2, month = m2, year = y2} =
         (y1 == y2) && (m1 == m2) && (d1 == d2)

instance Ord Birthday where
  compare (Birthday {day = d1, month = m1, year = y1})
          (Birthday {day = d2, month = m2, year = y2})
            | (y1 > y2) = GT
            | (y1 < y2) = LT
            | (y1 == y2) && (m1 > m2) = GT
            | (y1 == y2) && (m1 < m2) = LT
            | (y1 == y2) && (m1 == m2) && (d1 == d2) = EQ
            | (y1 == y2) && (m1 == m2) && (d1 > d2) = GT
            | (y1 == y2) && (m1 == m2) && (d1 < d2) = LT

data Month = January | February | March | April | May | June | July | August
           | September | October | November | December
           deriving (Show, Eq, Ord)

data Zodiac = Aries | Taurus | Gemini | Cancer | Leo | Virgo | Libra | Scorpius
            | Sagittarius | Capricorn | Aquarius | Pisces
            deriving (Show, Eq, Ord, Read)

type InfZodiac = Maybe Zodiac

zodiacSigns :: [String]
zodiacSigns = [ "Aries", "Taurus", "Gemini", "Cancer", "Leo", "Virgo", "Libra"
              , "Scorpius", "Sagittarius", "Capricorn", "Aquarius", "Pisces"
              , "Nothing" ]

fileName :: String
fileName = "biographies.list"
