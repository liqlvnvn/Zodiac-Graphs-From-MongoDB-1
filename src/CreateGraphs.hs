module CreateGraphs where

import Parsing.ParsingFromMongo

import Graphics.GChart

import qualified Graphics.Gnuplot.Advanced as GP

import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Frame.OptionSet.Style as OptsStyle
import qualified Graphics.Gnuplot.Frame.OptionSet.Histogram as Histogram

import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D

import qualified Graphics.Gnuplot.LineSpecification as LineSpec

import Data.Foldable (foldMap, )

import Data.Tuple.Select
import Numeric.Extra ( intToDouble )
import Data.List

import GHC.IO.Exception

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


--graph3 :: IO GHC.IO.Exception.ExitCode
graph3 inf = GP.plotDefault $ histogram2d inf

{-
histogram2d :: Frame.T (Graph2D.T Int Double)
histogram2d =
   Frame.cons (
      Opts.title "Comparison of how well software revisions perform on each hardware version" $
      Histogram.clusteredGap 2 $
      Opts.boxwidthAbsolute 0.9 $
      OptsStyle.fillBorderLineType (-1) $
      OptsStyle.fillSolid $
      Opts.keyOutside $
      Opts.xTicks2d
         [("220", 0), ("320", 1), ("420", 2), ("520", 3), ("620", 4), ("720", 5)] $
      Opts.deflt) $
   foldMap (\(title,dat) ->
      fmap (Graph2D.lineSpec (LineSpec.title title LineSpec.deflt)) $
      Plot2D.list Graph2D.histograms dat) $
   ("1.0011", [102, 213, 378, 408, 840,  920]) :
   ("1.0012", [152, 233, 388, 433, 850, 1200]) :
   ("1.0013", [172, 263, 408, 463, 860, 1500]) :
   ("1.0014", [192, 293, 458, 483, 890, 2000]) :
   []
 -}

prepareData :: [(String, String, Int)] -> [(String, [Int])]
prepareData dal = final $ switch $ transpose $ map bb (aa (sorting dal))--map cc (map bb (aa dal))
     where
         sorting :: [(String, String, Int)] -> [(String, String, Int)]
         sorting d = sortOn extr3 (sortOn extr3' d)
         aa :: [(String, String, Int)] -> [[(String, String, Int)]]
         aa d = groupBy comp d
             where
                 comp :: (String, String, Int) -> (String, String, Int) -> Bool
                 comp a b = sel1 a == sel1 b
         bb :: [(String, String, Int)] -> [(String,Int)]
         bb d = zip f s
             where
                 f = map (\x -> sel1 x ++ "/" ++ sel2 x) d
                 s = sel3 $ unzip3 d
         switch :: [[(String, Int)]] -> [[(String, Int)]]
         switch (x:xs) = x : reverse xs
         switch [] = []
         final :: [[(String, Int)]] -> [(String, [Int])]
         final (x:xs) = ("", map (\x -> snd x) x) : final xs
         final [] = []


        -- cc :: (String, Int) -> (String, [Double])
        -- cc d = (concat $ fst $ unzip d, map intToDouble (snd $ unzip d))


--histogram2d :: Frame.T (Graph2D.T Int Double)
histogram2d inf =
    Frame.cons (
        Opts.title "Comparison of how well software revisions perform on each hardware version" $
        Histogram.clusteredGap 2 $
        Opts.boxwidthAbsolute 0.9 $
        OptsStyle.fillBorderLineType (-1) $
        OptsStyle.fillSolid $
        Opts.keyOutside $
        Opts.xTicks2d
           [("220", 0), ("320", 1), ("420", 2), ("520", 3), ("620", 4), ("720", 5)] $
        Opts.deflt) $
    foldMap (\(title,dat) ->
        fmap (Graph2D.lineSpec (LineSpec.title title LineSpec.deflt)) $
        Plot2D.list Graph2D.histograms dat) $ prepareData inf
    {-
    ("1.0011", [102, 213, 378, 408, 840,  920]) :
    ("1.0012", [152, 233, 388, 433, 850, 1200]) :
    ("1.0013", [172, 263, 408, 463, 860, 1500]) :
    ("1.0014", [192, 293, 458, 483, 890, 2000]) :
    []
    -}
