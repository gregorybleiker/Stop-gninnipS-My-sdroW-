splitAtBlank :: String -> String -> (String, String)
splitAtBlank [] x = (x, [])
splitAtBlank (x:xs) r
  | x == ' ' = (r, xs)
  | otherwise = splitAtBlank xs (r++[x])

conditionalReverse :: String -> String
conditionalReverse x
  | length x > 4 = reverse x
  | otherwise = x

spinWords :: String -> String
spinWords [] = []
spinWords x 
  | v == [] = (conditionalReverse $ u)
  | otherwise = (conditionalReverse $ u) ++ " " ++ (spinWords $ v)
  where (u,v) = splitAtBlank x []

main :: IO ()
main = do 
  let result = spinWords "Stop Spinning My Words"
  print result