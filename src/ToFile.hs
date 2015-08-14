module ToFile where

import Parsing.ParsingFromMongo
import Parsing

import Data.Tuple.Select

convert1 :: [(String, Int)] -> String
convert1 list = foldr1 (++) $ map conv (sortOn extr1 list)
    where
        conv :: (String, Int) -> String
        conv tpl = fst tpl ++ " " ++ (show . snd) tpl ++ "\n"

convert2 :: [(String, Int)] -> String
convert2 list = foldr1 (++) $ map conv (sortOn extr2 list)
    where
        conv :: (String, Int) -> String
        conv tpl = fst tpl ++ " " ++ (show . snd) tpl ++ "\n"

convert3 :: [(String, String, Int)] -> String
convert3 list = foldr1 (++) $ map conv (switch2And3 $ sorting list)
    where
        sorting :: [(String, String, Int)] -> [(String, String, Int)]
        sorting d = sortOn extr3 (sortOn extr3' d)
        switch2And3 :: [(String, String, Int)] -> [(String, String, Int)]
        switch2And3 (a:b:c:xs) = a:c:b:switch2And3 xs
        switch2And3 []         = []
        switch2And3 [a]        = [a]
        switch2And3 [a, b]     = [a, b]
        conv :: (String, String, Int) -> String
        conv tpl = sel1 tpl ++ "/" ++ sel2 tpl ++ " "
                   ++ (show . sel3) tpl ++ "\n"

convert4 :: [(Birthday, Int)] -> String
convert4 list = foldr1 (++) $ map conv (sorting list)
    where
        sorting :: [(Birthday, Int)] -> [(Birthday, Int)]
        sorting d = sortOn extr4 d
        conv :: (Birthday, Int) -> String
        conv tpl = (show . day . fst) tpl ++ " " ++ (show . month . fst) tpl
            ++ " " ++ (show . snd) tpl ++ "\n"
