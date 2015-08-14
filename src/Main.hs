module Main where

import ReadFromMongo
import Parsing.ParsingFromMongo
import ToFile

import Data.List (sort)

main :: IO ()
main = do
    a' <- readFromMongo db (query1 collection)
    b' <- readFromMongo db (query2 collection)
    c' <- readFromMongo db (query3 collection)
    d' <- readFromMongo db (query4 collection)
    let a = parse1 a'
    let b = parse1 b'
    let c = parse2 c'
    let d = parse3 d'
    print a'
    print $ a
    writeFile "file1.txt" (convert1 a)
    -- print $ graph2 "Second" b
    -- graph3 c
    -- print $ sortOn extr4 d
    print "finish"
