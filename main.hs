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
splitAtBlank2 [] x = ([], x)
splitAtBlank2 (x:xs) r
  | x == ' ' = (r, xs)
  | otherwise = splitAtBlank2 xs (r++[x])

reverseWord :: String -> String
reverseWord [] = []
reverseWord (x:xs) = reverseWord xs ++ [x]

main :: IO ()
main = do 
  let result = splitAtBlank (SplitResult "abcd bc def" [])
  let result4 = reverseWord $ word result
  let result2 = splitAtBlank2 "abcd bc def" []
  let result3 = reverseWord $ fst result2
  print result4