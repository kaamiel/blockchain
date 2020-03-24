module PPrint where

writeln :: String -> IO ()
writeln = putStrLn

showsPair :: Show a => (String, a) -> ShowS
showsPair (k, v) = showString k . showString ": " . shows v

pprH, pprV :: [ShowS] -> ShowS
pprV = intercalateS $ showChar '\n'
pprH = intercalateS $ showChar ' '

intercalateS :: ShowS -> [ShowS] -> ShowS
intercalateS sep list = foldr1 (\a b -> a . sep . b) list

pprListWith :: (a -> ShowS) -> [a] -> ShowS
pprListWith f list = intercalateS (showChar '\n') . map f $ list

runShows :: ShowS -> IO ()
runShows = putStrLn . ($"")
