{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module ReadFromMongo where

import Database.MongoDB

db :: Database
db = "Actors"

collection :: Collection
collection = "actors"

readFromMongo :: Database -> Action IO [Document] -> IO [Document]
readFromMongo db query = do
    pipe <- connect (host "127.0.0.1")
    d <- access pipe master db query
    close pipe
    return d

-- | db.Actors.aggregate([{$group: {_id: "$zodiac",count: {$sum: 1}}}])
query1 :: Collection -> Action IO [Document]
query1 col = aggregate col q
    where
        q = [[ "$group" =: [ "_id" =: String "$zodiac"
                           , "count" =: ["$sum" =: Int32 1]]]]

-- | db.Actors.aggregate([{$group: {_id: "$inf_zodiac",count: {$sum: 1}}}])
query2 :: Collection -> Action IO [Document]
query2 col = aggregate col q
    where
        q = [["$group" =: [ "_id" =: String "$inf_zodiac"
                          , "count" =: ["$sum" =:  Int32 1]]]]

query3 :: Collection -> Action IO [Document]
query3 col = aggregate col q
    where
        q = [["$group" =: [ "_id" =: [ "zodiac" =: String "$zodiac"
                                     , "inf_zodiac" =: String "$inf_zodiac"]
                          , "count" =: ["$sum" =:  Int32 1]]]]

-- | distribution of birthdays
query4 :: Collection -> Action IO [Document]
query4 col = aggregate col q
    where
        q = [["$group" =: [ "_id" =: [ "day" =: String "$birthday.day"
                                     , "month" =: String "$birthday.month"]
                          , "count" =: ["$sum" =:  Int32 1]]]]

-- | number of days with b-d's
query5 :: Collection -> Action IO [Document]
query5 col = aggregate col q
    where
        q = [ ["$group" =: ["_id" =: [ "day" =: String "$birthday.day"
                                     , "month" =: String "$birthday.month"]
                                     , "count" =: ["$sum" =:  Int32 1]]]
            , ["$group" =: [ "_id" =: Int32 0
                           , "count" =: ["$sum" =: Int32 1]]]]
