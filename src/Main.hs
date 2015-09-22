module Main where

import ReadFromMongo            ( readFromMongo, db, collection,
                                  query1, query2, query3, query4 )
import Parsing.ParsingFromMongo ( parse1, parse2, parse3, parse4 )
import ToFile                  -- ( convert1, convert2, convert3, convert4 )
import SimpleAnalysis

import System.Directory

main :: IO ()
main = do
  -- Collect data from database
  a' <- readFromMongo db (query1 collection)
  b' <- readFromMongo db (query2 collection)
  c' <- readFromMongo db (query3 collection)
  d' <- readFromMongo db (query4 collection)
  -- Converting raw db answer to internal representation
  let a = parse1 a'
  let b = parse2 b'
  let c = parse3 c'
  let d = parse4 d'
  -- Writing results
  createDirectoryIfMissing False "data"
  setCurrentDirectory "data"
  -- Make data files for gnuplot
  writeFile "data1.txt" (convert1 a)
  writeFile "data2.txt" (convert2 b)
  writeFile "data3.txt" (convert3 c)
  writeFile "data4.txt" (convert4 d)
  -- Analysis of graphs
  writeFile "graph1.txt" (mkAnalysisOf1stGraph a)
  writeFile "graph2.txt" (mkAnalysisOf2ndGraph b)
  writeFile "graph3.txt" (mkAnalysisOf3rdGraph c)
  writeFile "graph4.txt" (mkAnalysisOf4rdGraph d)
