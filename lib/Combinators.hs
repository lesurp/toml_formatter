module Combinators where

import ParserBase

takeCharP :: Char -> String -> (String, Maybe Char)
takeCharP c s = case s of (c2:ss) | c2 == c   -> (ss, Just c)
                                  | otherwise -> (ss, Nothing)
                          _ -> (s, Nothing)
takeChar :: Char -> Parser Char
takeChar c = P $ \s -> case takeCharP c s of (s, Just c)  -> (s, Right c)
                                             (s, Nothing) -> (s, Left "Niooo")

takeWhileRecP out f [] = ([], out)
takeWhileRecP out f s = case f s of (s, Just a) -> takeWhileRecP (a:out) f s
                                    (s, Nothing) -> (s, out)
takeWhileRec f = P $ \s -> case takeWhileRecP [] f s of (s, a) -> (s, Right a)
takeWhitespace = P $ \s -> (takeWhitespaceP s, Right ())

takeUntilP :: Char -> String -> (String, String)
takeUntilP c (cc:ss) = if cc == c then ([], c:ss) else (cc:nextChars, leftovers)
                            where (nextChars, leftovers) = takeUntilP c ss
takeUntilP c [] = error $ "Character not found: " ++ show c
takeUntil c = P $ \s -> let (taken, leftovers) = takeUntilP c s in (leftovers, Right taken)

takeOneP :: String -> (String, Char)
takeOneP (c:ss) = (ss, c)
takeOne = P $ \s -> let (leftovers, taken) = takeOneP s in (leftovers, Right taken)

takeUntilSkip c = const <$> takeUntil c <*> takeOne

splitIf :: (Char -> Bool) -> String -> (String, String)
splitIf = splitIfRec []

splitIfRec:: String -> (Char -> Bool) -> String -> (String, String)
splitIfRec out f [] = (out, [])
splitIfRec out f (h:t) = if f h then splitIfRec (out ++ [h]) f t
                          else (out, h:t)

isTomlKey c | c == ']' = False
            | c == ' ' = False
            | c == '=' = False
            | otherwise = True

takeWord :: Parser String
takeWord = P $ \s -> let (word, leftovers) = splitIf isTomlKey s in (leftovers, Right word)
