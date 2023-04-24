module Parser where

import ParserBase
import Combinators

type Value = String
type Key = String
newtype Comment = C String deriving (Eq, Show)
data Node = Indent Int | Comment Comment | EmptyLine | Assignment Key Value (Maybe Comment) | Table Key (Maybe Comment) [Node] deriving (Eq, Show)
newtype RuleSet = Rs { r:: String -> (String, Maybe Node) }
type Document = [Node]

instance Semigroup RuleSet where
    (Rs f) <> (Rs g) = Rs $ \s -> case f s of (s, Just ff) -> (s, Just ff)
                                              (_, Nothing) -> g s
parseNotTable = Rs parseComment <> Rs parseEmptyLine <> Rs parseAssignment
parseNode = parseNotTable <> Rs parseTable


trimHead (' ':s) = s
trimHead s = s

trimTail = reverse . trimHead . reverse
trimPreCommentP :: String -> String
trimPreCommentP (c:s) = if c == ' ' || c == '#' then trimPreCommentP s
                        else c:s

trimPreComment :: Parser ()
trimPreComment = P $ \s -> (trimPreCommentP s, Right ())

takeComment = skipAndTrimTail <$> takeWhitespace <*> takeChar '#' <*> trimPreComment <*> takeUntilSkip '\n'
                where skipAndTrimTail _ _ _ = trimTail

try :: Parser a -> Parser (Maybe a)
try (P p) = P $ \s -> case p s of (s, Left err) -> (s, Right Nothing)
                                  (s, Right a) -> (s, Right $ Just a)

parseComment :: String -> (String, Maybe Node)
parseComment s = case p s of (s, Left err) -> (s, Nothing)
                             (s, Right c) -> (s, Just $ Comment (C c))
                            where (P p) = takeComment

takeEmptyLine = (,) <$> takeWhitespace <*> takeChar '\n'

parseEmptyLine :: String -> (String, Maybe Node)
parseEmptyLine s = case p s of (s, Left err) -> (s, Nothing)
                               (s, Right _) -> (s, Just EmptyLine)
                            where (P p) = takeEmptyLine


takeAssignement :: Parser (String, String, Maybe String)
takeAssignement = dropStuff <$> takeWhitespace <*> takeWord <*> takeChar '=' <*> takeWord <*> try takeComment
                        where dropStuff _ k _ v c = (k, v, c)

parseAssignment :: String -> (String, Maybe Node)
parseAssignment s = case p s of (s, Left err) -> (s, Nothing)
                                (s, Right (key, value, c)) -> (s, Just $ Assignment key value (fmap C c))
                        where (P p) = takeAssignement

takeTableHeader :: Parser (String, Maybe String)
takeTableHeader = dropStuff <$> takeWhitespace <*> takeChar '[' <*> takeUntilSkip ']' <*> try takeComment
                      where dropStuff _ _ header c = (header, c)
parseTable :: String -> (String, Maybe Node)
parseTable s = case p s of (s, Left err) -> (s, Nothing)
                           (s, Right (header, Nothing)) -> takeTableComplete header Nothing s
                           (s, Right (header, Just c)) -> takeTableComplete header (Just (C c)) s
                        where (P p) = takeTableHeader

from Nothing = []
from (Just n) = n

takeTableComplete :: String -> Maybe Comment -> String -> (String, Maybe Node)
takeTableComplete h c s = (left, Just $ Table h c nodes)
                                    where (left, nodes) = tryRecRuleset parseNotTable s

tryRecRuleset :: RuleSet -> String -> (String, [Node])
tryRecRuleset = tryRecRulesetRec []
tryRecRulesetRec :: [Node] -> RuleSet -> String -> (String, [Node])
tryRecRulesetRec out (Rs r) s = case r s of (s, Just n) -> tryRecRulesetRec (n:out) (Rs r) s
                                            (s, Nothing) -> (s, out)

parseToml :: String -> Document
parseToml = parseTomlRec []

parseTomlRec :: [Node] -> String -> Document
parseTomlRec acc [] = acc
parseTomlRec acc s = case parseNodeF s of (leftovers, Just n) -> parseTomlRec (n:acc) leftovers
                                          (leftovers, Nothing) -> error "Could not parse: " leftovers
                                where (Rs parseNodeF) = parseNode
