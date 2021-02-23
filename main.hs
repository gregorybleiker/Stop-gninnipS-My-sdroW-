splitAtBlank :: String -> String -> (String, String)
splitAtBlank [] x = (x, [])
splitAtBlank (x:xs) r
  | x == ' ' = (r, xs)
  | otherwise = splitAtBlank xs (r++[x])

reverseWord :: String -> String
reverseWord [] = []
reverseWord (x:xs) = reverseWord xs ++ [x]

conditionalReverse :: String -> String
conditionalReverse x
  | length x > 4 = reverseWord x
  | otherwise = x

wordList :: String -> String
wordList [] = []
wordList x = do
  let result = splitAtBlank x [] 
  (conditionalReverse $ fst result) ++ " " ++ (wordList $ snd result)

main :: IO ()
main = do 
  let result = wordList "Stop Spinning My Words"
  print result