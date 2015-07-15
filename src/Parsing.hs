module Parsing where

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
    Birthday {day = day1, month = month1, year = year1} ==
        Birthday {day = day2, month = month2, year = year2} =
            (day1 == day2) && (month1 == month2)

instance Ord Birthday where
    compare (Birthday {day = day1, month = month1, year = year1})
            (Birthday {day = day2, month = month2, year = year2})
                | (month1 > month2) = GT
                | (month1 < month2) = LT
                | (month1 == month2) && (day1 == day2) = EQ
                | (month1 == month2) && (day1 > day2) = GT
                | (month1 == month2) && (day1 < day2) = LT

data Month = January | February | March | April | May | June | July | August
           | September | October | November | December
           deriving (Show, Eq, Ord)

data Zodiac = Aries | Taurus | Gemini | Cancer | Leo | Virgo | Libra | Scorpius
            | Sagittarius | Capricorn | Aquarius | Pisces
            deriving (Show, Eq, Ord)

zodiacSigns :: [String]
zodiacSigns = [ "Aries", "Taurus", "Gemini", "Cancer", "Leo", "Virgo", "Libra"
              , "Scorpius", "Sagittarius", "Capricorn", "Aquarius", "Pisces"
              , "Nothing" ]

fileName :: String
fileName = "biographies.list"
