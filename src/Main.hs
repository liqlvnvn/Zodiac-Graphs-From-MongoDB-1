module Main where

import ReadFromMongo            ( readFromMongo, db, collection,
                                  query1, query2, query3, query4 )
import Parsing.ParsingFromMongo ( parse1, parse2, parse3, parse4 )
import ToFile                   ( convert1, convert2, convert3, convert4 )
import System.FilePath          ( (</>) )

main :: IO ()
main = do
  a' <- readFromMongo db (query1 collection)
  b' <- readFromMongo db (query2 collection)
  c' <- readFromMongo db (query3 collection)
  d' <- readFromMongo db (query4 collection)
  let a = parse1 a'
  let b = parse2 b'
  let c = parse3 c'
  let d = parse4 d'
  writeFile ("data" </> "file1.txt") (convert1 a)
  writeFile ("data" </> "file2.txt") (convert2 b)
  writeFile ("data" </> "file3.txt") (convert3 c)
  writeFile ("data" </> "file4.txt") (convert4 d)
