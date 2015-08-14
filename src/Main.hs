module Main where

import ReadFromMongo
import Parsing.arsingFromMongo
import ToFile

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
    writeFile "file1.txt" (convert1 a)
    writeFile "file2.txt" (convert2 b)
    writeFile "file3.txt" (convert3 c)
    writeFile "file4.txt" (convert4 d)
    --print d
