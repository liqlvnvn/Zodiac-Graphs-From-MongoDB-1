module CreateGraphs where

import Parsing.ParsingFromMongo

import Graphics.GChart
-- import Graphics.EasyPlot
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

import Data.Tuple.Select
import Numeric.Extra ( intToDouble )
import Data.List

graph1 :: String -> [(String, Int)] -> String
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
graph2 :: String -> [(String, Int)] -> String
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
{-}
-- | For second graph
--graph3 :: String -> [(String, String, Int)] -> String
graph3 title rec = createGraph3 title records label
    where rec' = sortOn extr3 (sortOn extr3' rec)
          label = label2 rec'
          records = data2 rec'
-}
{-}
-- createGraph3 :: String -> [Float] -> [String] -> String
createGraph3 title d labels =  plot X11 $ Data2D [Title title]
                                                 []
                                                 [(x, y) | x <- [1..37], y <- d]
                                                 where n = length d
-}

titles = ["Cash","Equity"]
{-}
values :: [ (String,[Double]) ]
values =
    [ ("Jun", [20,45])
    , ("Jul", [45,30])
    , ("Aug", [30,20])
    , ("Sep", [10,40])
    , ("Oct", [20,50])
    ]
-}
prepareData :: [(String, String, Int)] -> [(String, [Double])]
prepareData dal = cc $ map bb (aa dal)
    where aa :: [(String, String, Int)] -> [[(String, String, Int)]]
          aa d = groupBy comp d
          bb :: [(String, String, Int)] -> [(String,Int)]
          bb d = map (\ x -> sel1 x ++ "/" ++ sel2 x) d
          cc :: [(String,Int)] -> (String,[Double])
          cc d = (concat $ fst $ unzip d, map intToDouble (snd $ unzip d))
          comp :: (String, String, Int) -> (String, String, Int) -> Bool
          comp a b = fst a == fst b

gr3 values = toFile def "example11_big.png" $ do
    layout_title .= "Sample Bars"
    layout_title_style . font_size .= 10
    layout_x_axis . laxis_generate .= autoIndexAxis (map fst values)
    plot $ fmap plotBars $ bars titles (addIndexes (map snd values))
{-
plot X11 $ Data2D [Title "Sample Data"] [] [(1, 2), (2, 4), ...]

createGraph2 = getChartUrl $ do
    setChartSize 500 400
    setChartTitle "Example of Plotting a Pie Chart in Haskell"
    setDataEncoding simple
    addChartData (nums :: [Int])
--    setLabels (lines nums)
    setChartType Pie
-}
