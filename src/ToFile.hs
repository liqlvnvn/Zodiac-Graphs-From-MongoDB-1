module ToFile where

convert1 :: [(String, Int)] -> String
convert1 list = foldr1 (++) $ map conv list
    where
        conv :: (String, Int) -> String
        conv tpl = fst tpl ++ " " ++ (show . snd) tpl ++ "\n"
