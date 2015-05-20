{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module ReadFromMongo where

import Parsing
import Parsing.ParsingFromMongo

import Database.MongoDB
import Data.Bson

readFromMongo1 :: IO [Document]
readFromMongo1 = do
    pipe <- connect (host "127.0.0.1")
    a <- access pipe master "Actors" run1
    close pipe
    return a

readFromMongo2 :: IO [Document]
readFromMongo2 = do
    pipe <- connect (host "127.0.0.1")
    b <- access pipe master "Actors" run2
    close pipe
    return b

readFromMongo3 :: IO [Document]
readFromMongo3 = do
    pipe <- connect (host "127.0.0.1")
    c <- access pipe master "Actors" run3
    close pipe
    return c

readFromMongo4 :: IO [Document]
readFromMongo4 = do
    pipe <- connect (host "127.0.0.1")
    d <- access pipe master "Actors" run4
    close pipe
    return d

-- | db.actors.aggregate([{$group: {_id: "$zodiac",count: {$sum: 1}}}])
run1 :: Action IO [Document]
run1 = aggregate "actors" query1

query1 :: [Document]
query1 = [[ "$group" =: [ "_id" =: "$zodiac"
                        , "count" =: ["$sum" =:  1]]]]

-- | db.actors.aggregate([{$group: {_id: "$inf_zodiac",count: {$sum: 1}}}])
run2 :: Action IO [Document]
run2 = aggregate "actors" query2

query2 :: [Document]
query2 = [["$group" =: [ "_id" =: "$inf_zodiac"
                       , "count" =: ["$sum" =:  1]]]]

run3 :: Action IO [Document]
run3 = aggregate "actors" query3

query3 :: [Document]
query3 = [["$group" =: [ "_id" =: [ "zodiac" =: "$zodiac"
                                  , "inf_zodiac" =: "$inf_zodiac"]
                       , "count" =: ["$sum" =:  1]]]]

-- | distribution of birthdays
run4 :: Action IO [Document]
run4 = aggregate "actors" query4

query4 :: [Document]
query4 = [["$group" =: [ "_id" =: [ "day" =: "$birthday.day"
                                  , "month" =: "$birthday.month"]
                       , "count" =: ["$sum" =:  1]]]]

-- | number of days with b-d's
run5 :: Action IO [Document]
run5 = aggregate "actors" query5

query5 :: [Document]
query5 = [ ["$group" =: ["_id" =: [ "day" =: "$birthday.day"
                                  , "month" =: "$birthday.month"]
                        , "count" =: ["$sum" =:  1]]]
         , ["$group" =: ["_id" =: 0, "count" =: ["$sum" =:  1]]]]
