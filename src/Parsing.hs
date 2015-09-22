{-| Data types for describing actions.
-}

{-# LANGUAGE RecordWildCards #-}

module Parsing
  ( Person(..)
  , Birthday(..)
  , Month
  , Zodiac
  , InfZodiac
  , StatsSign
  , StatsInfSign
  , StatsExactSign
  , StatsBirthday
  , fileName
  , stringToZodiac
  ) where

import Text.Printf

data Person = Person
  { firstName :: String
  , lastName  :: String
  } deriving Show

data Birthday = Birthday
  { day   :: Int
  , month :: Month
  , year  :: Int
  } 

instance Show Birthday where
  show Birthday {..} = show month ++ " " ++ printf "%2d" day

instance Eq Birthday where
  (==) Birthday {day = d1, month = m1, year = y1}
       Birthday {day = d2, month = m2, year = y2} =
         (y1 == y2) && (m1 == m2) && (d1 == d2)

instance Ord Birthday where
  compare (Birthday {day = d1, month = m1, year = y1})
          (Birthday {day = d2, month = m2, year = y2})
            | y1 > y2 = GT
            | y1 < y2 = LT
            | (y1 == y2) && (m1 > m2) = GT
            | (y1 == y2) && (m1 < m2) = LT
            | (y1 == y2) && (m1 == m2) && (d1 == d2) = EQ
            | (y1 == y2) && (m1 == m2) && (d1 > d2) = GT
            | (y1 == y2) && (m1 == m2) && (d1 < d2) = LT
  compare Birthday{} Birthday{} = EQ

data Month = January | February | March | April | May | June | July | August
           | September | October | November | December
           deriving (Eq, Ord, Read)

instance Show Month where
  show January   = "Jan"
  show February  = "Feb"
  show March     = "Mar"
  show April     = "Apr"
  show May       = "May"
  show June      = "Jun"
  show July      = "Jul"
  show August    = "Aug"
  show September = "Sep"
  show October   = "Oct"
  show November  = "Nov"
  show December  = "Dec"

data Zodiac = Aries | Taurus | Gemini | Cancer | Leo | Virgo | Libra | Scorpius
            | Sagittarius | Capricorn | Aquarius | Pisces
            deriving (Eq, Ord, Read)

instance Show Zodiac where
  show Aries       = "ARI"
  show Taurus      = "TAU"
  show Gemini      = "GEM"
  show Cancer      = "CAN"
  show Leo         = "LEO"
  show Virgo       = "VIR"
  show Libra       = "LIB"
  show Scorpius    = "SCO"
  show Sagittarius = "SAG"
  show Capricorn   = "CAP"
  show Aquarius    = "AQU"
  show Pisces      = "PIS"

type InfZodiac = Maybe Zodiac
type NumberOfEntries = Int

type StatsSign      = (Zodiac, NumberOfEntries)
type StatsInfSign   = (InfZodiac, NumberOfEntries)
type StatsExactSign = (Zodiac, InfZodiac, NumberOfEntries)
type StatsBirthday  = (Birthday, NumberOfEntries)

fileName :: String
fileName = "biographies.list"

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
  _             -> error "Can't convert string to zodiac sign! (function: stringToZodiac)"
