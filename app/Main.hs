module Main where

import Parser

main :: IO ()
main = interact formatToml

formatToml :: String -> String
formatToml s = formatDocument $ parseToml s

-- TODO

formatDocument :: Document -> String
formatDocument = concatMap formatNode . reverse

formatNode :: Node -> String
formatNode (Comment (C c)) = "# " ++ c
formatNode EmptyLine = "\n"
formatNode (Assignment k v Nothing) = k ++ " = " ++ v
formatNode (Assignment k v (Just (C c))) = k ++ " = " ++ v ++ " # " ++ c
formatNode (Table k Nothing nodes) = "[" ++ k ++ "]\n" ++ formatTableNodes nodes
formatNode (Table k (Just (C c)) nodes) = "[" ++ k ++ "] # " ++ c ++ "\n" ++ formatTableNodes nodes

formatTableNodes :: [Node] -> String
formatTableNodes [] = ""
formatTableNodes (EmptyLine:ns) = formatTableNodes ns
formatTableNodes (n:ns) = "\t" ++ formatNode n ++ formatTableNodes ns
