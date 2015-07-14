module Main where

import ReadFromMongo
import CreateGraphs
import Parsing.ParsingFromMongo

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
    print $ graph1 "First" a
    print $ graph2 "Second" b
    graph3 c
    print "finish"
