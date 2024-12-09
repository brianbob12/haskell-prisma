module FormatString where

import Data.List (span)

-- Replaces all the variable names inside ${...} with the corresponding values
-- If a variable is not found, it is left as is
formatString :: String -> [(String, String)] -> String

formatString [] vars = [] 
formatString ('$' : '{' : rest) vars = 
  let (var, closingBraceAndAfter) = span (/= '}') rest
  in case lookup var vars of
    Just value -> value ++ (formatString (tail closingBraceAndAfter) vars)
    Nothing -> "${" ++ var ++ (formatString closingBraceAndAfter vars)

formatString (c : rest) vars = 
  c : (formatString rest vars)


