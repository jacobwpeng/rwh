#!/usr/bin/env ghci
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a:_) = Just a

myHead :: [a] -> a
myHead (a:_) = a


myTail :: [a] -> [a]
myTail (_:rest) = rest

myLast :: [a] -> a
myLast as = myHead $ reverse as

myInit :: [a] -> [a]
myInit (a:as) 
    | length as == 1 = [a]
    | otherwise = a : myInit as

safeTail :: [a] -> Maybe [a]
safeTail as
 | null as = Nothing
 | otherwise = Just $ tail as

safeLast :: [a] -> Maybe a
safeLast as
 | null as = Nothing
 | otherwise = Just $ last as

notNull :: [a] -> Bool
notNull as = not (null as)

myWordsInternal :: [Char] -> [[Char]]
myWordsInternal as = 
    let (pre, suf) = break isWhiteSpace as
    in pre : case suf of
         (' ':rest) -> myWords rest
         ('\r':rest) -> myWords rest
         ('\n':rest) -> myWords rest
         ('\t':rest) -> myWords rest
         rest -> [rest]

myWords :: [Char] -> [[Char]]
myWords as = filter (notNull) (myWordsInternal as)

isWhiteSpace c = c == ' ' || c == '\r' || c == '\n' || c == '\t'

splitWith :: (a->Bool) -> [a] -> [[a]]
splitWith pred [] = []
splitWith pred as = 
    let (pre, suf) = break pred as
        rest = case suf of
                    (a:r) -> r
                    _ -> []
    in if length pre == 0 
       then splitWith pred rest
       else pre : splitWith pred rest
