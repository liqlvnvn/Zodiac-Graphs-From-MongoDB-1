module CreateGraphs where

import Parsing.ParsingFromMongo

import Graphics.GChart

type Url = String

graph1 :: String -> [(String, Int)] -> Url
graph1 title rec = createGraph1 title records label
    where rec' = sortOn extr1 rec
          label = label1 rec'
          records = data1 rec'

--"Распределение актеров по знаку зодиака"
createGraph1 :: String -> [Float] -> [String] -> String
createGraph1 title d labels = getChartUrl $ do
    setChartSize 600 450
    setChartTitle title
    setDataEncoding text
    addChartData d
    setLabels labels
    setChartType Pie

-- | For second graph
graph2 :: String -> [(String, Int)] -> Url
graph2 title rec = createGraph2 title records label
    where rec' = sortOn extr2 rec
          label = label1 rec'
          records = data1 rec'

createGraph2 :: String -> [Float] -> [String] -> String
createGraph2 title d labels = getChartUrl $ do
    setChartSize 600 450
    setChartTitle title
    setDataEncoding text
    addChartData d
    setLabels labels
    setChartType Pie
