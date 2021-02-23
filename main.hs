conditionalReverse :: String -> String
conditionalReverse x
  | length x > 4 = reverse x
  | otherwise = x

spinWords :: String -> String
spinWords [] = []
spinWords x 
  | v == [] = (conditionalReverse u)
  | otherwise = (conditionalReverse u) ++ " " ++ (spinWords $ unwords v)
  where (u:v) = words x

main :: IO ()
main = do 
  let result = spinWords "Stop Spinning My Words"
  print result