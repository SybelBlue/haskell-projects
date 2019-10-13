main = do
  line <- getLine
  if null line
    then return ()
    else do
      putStrLn $ reverseWords line
      main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
-- equivalent to
-- reverseWords x = unwords ((map reverse) (words x))
