module Main where

import ReadFromMongo
import CreateGraphs
import Parsing.ParsingFromMongo

import Data.List
import Parsing

main :: IO ()
main = do
    a' <- readFromMongo1
    b' <- readFromMongo2
    c' <- readFromMongo3
    d' <- readFromMongo4
    let a = parse1 a'
    let b = parse1 b'
    let c = parse2 c'
    let d = parse3 d'
    --print c
    let s =  sortOn extr3 (sortOn extr3' c)
    --t <- graph3 "3" c
    --print t
    gr3 $ prepareData s
    -- print $ sortOn extr1 b
    -- print $ sortDoc a
    -- print a
    -- putStrLn $ graph1 "123" a   -- link on first graph
    -- putStrLn $ graph1 "123" b   -- link on first graph
    -- print $ data1 a'
    --putStrLn createGraph1
