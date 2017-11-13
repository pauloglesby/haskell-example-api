module Wiggly.Util.Json
       (
         recordNameToJsonName
       ) where

import Data.Char (isUpper, toLower)

recordNameToJsonName :: String -> String
recordNameToJsonName xs = let suffix = dropWhile (not . isUpper) xs
                              camel = (toLower (head suffix)) : tail suffix
                          in camelToSnake camel

camelToSnake :: String -> String
camelToSnake [] = []
camelToSnake xs@(_:[]) = xs
camelToSnake (x:xs) = if isUpper x
                      then '_' : ( (toLower x) : (camelToSnake xs) )
                      else x : (camelToSnake xs)
