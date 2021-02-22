data SplitResult = SplitResult { 
  word ::String,
  rest :: String 
  } deriving (Show)

splitAtBlank :: SplitResult -> SplitResult
splitAtBlank (SplitResult [] []) = (SplitResult [] [])
splitAtBlank (SplitResult [] x) = (SplitResult [] x)
splitAtBlank (SplitResult (x:xs) r)
  | x == ' ' = SplitResult r xs
  | otherwise = splitAtBlank (SplitResult xs (r++[x]))

splitAtBlank2 :: String -> String -> (String, String)
splitAtBlank2 [] x = (x, [])
splitAtBlank2 (x:xs) r
  | x == ' ' = (r, xs)
  | otherwise = splitAtBlank2 (r++[x]) xs

reverseWord :: String -> String
reverseWord [] = []
reverseWord (x:xs) = reverseWord xs ++ [x]


wordList :: String -> [String]
wordList [] = []
wordList x = do
  let result = splitAtBlank2 x [] 
  [fst result] ++ (wordList $ snd result)

main :: IO ()
main = do 
  let result = splitAtBlank (SplitResult "abcd bc def" [])
  let result4 = reverseWord $ word result
  let result2 = splitAtBlank2 "abcd def geg" []
  let result3 = reverseWord $ fst result2
  let result5 = wordList "abcd"
  print result2