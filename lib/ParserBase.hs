module ParserBase where

--import Control.Applicative

type Error = String
newtype Parser a = P { unP :: String -> (String, Either Error a) }

instance Functor Parser where
  fmap f (P st) = P $ \stream -> case st stream of
    (res, Left err) -> (res, Left err)
    (res, Right a ) -> (res, Right (f a))

takeWhitespaceP :: String -> String
takeWhitespaceP (' ':ss) = takeWhitespaceP ss
takeWhitespaceP ('\t':ss) = takeWhitespaceP ss
takeWhitespaceP s = s

instance Applicative Parser where
  pure a = P $ \stream -> (stream, Right a)
  P ff <*> P xx = P $ \stream0 -> case ff stream0 of   -- produce an f
    (stream1, Left err) -> (stream1, Left err)
    (stream1, Right f ) -> case xx (takeWhitespaceP stream1) of          -- produce an x
      (stream2, Left err) -> (stream2, Left err)
      (stream2, Right x ) -> (stream2, Right (f x))    -- return (f x)


(P a) <|> (P b) = P $ \s -> case a s of
                        (sleft, Right a) -> (sleft, Right (Left a))
                        (_, Left err) -> case b s of
                            (sleft, Right b) -> (sleft, Right (Right b))
                            (_, Left err) -> (s, Left "Neither worked")
