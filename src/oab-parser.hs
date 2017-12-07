{-# LANGUAGE DeriveGeneric #-}
import GHC.Generics

import System.Environment
import Data.Char
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec.Char hiding (letter)
import Text.ParserCombinators.Parsec.Prim
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.List.Split

--
-- utils
notNull :: String -> Bool
notNull = not . null

--
-- parsers
natural :: Parser Int
natural = do spaces
             ns <- many1 digit
             spaces
             return (read ns :: Int)

lexeme :: Parser a -> Parser a
lexeme p = do spaces
              v <- p
              spaces
              return v

word :: Parser String
-- if newline scheme is \r\n, won't work. if \n \n instead of \n\n,
-- won't work.
-- maybe should catch only \n\n, which are used as paragraph separator
word = do spaces
          w <- many1 $ satisfy (\c -> not $ isSpace c)
          ns <- many $ satisfy (\c -> c == '\n')
          spaces
          return (w ++ ns)

symbol :: String -> Parser String
symbol xs = lexeme (string xs)

manyTill' :: Parser a -> Parser b -> Parser [a]
manyTill' p end = do
  result <- manyTill p (lookAhead $ try $ end)
  end
  return result

--
-- data types
data Exam = Exam { year      :: Int,
                   edition   :: String,
                   questions :: [Question]
                 } deriving (Generic,Show)

instance ToJSON Exam where
  toEncoding = genericToEncoding defaultOptions

data Question = Question { number :: Int,
                           valid  :: Bool,
                           enum   :: String,
                           items  :: [Item]
                         } deriving (Generic,Show)

instance ToJSON Question where
    toEncoding = genericToEncoding defaultOptions

data Item = Item { letter  :: Letter,
                   correct :: Bool,
                   text    :: String
                 } deriving (Generic,Show)

instance ToJSON Item where
    toEncoding = genericToEncoding defaultOptions

data Letter = A | B | C | D deriving (Generic,Show,Read,Enum)

instance ToJSON Letter where
    toEncoding = genericToEncoding defaultOptions

--
-- OAB parsers
examQuestions :: Parser [Question]
examQuestions = do qs <- manyTill' (lexeme question) (lexeme eof)
                   return qs

question :: Parser Question
question = do symbol "ENUM"
              valid <- option [] (string "NULL")
              symbol "Questão"
              number <- natural
              enumWords <- manyTill' word (string "OPTIONS")
              is <- lexeme itemsP
              return Question {number=number, valid=(notNull valid),
                               enum=(unwords enumWords), items=is}

itemsP :: Parser [Item]
itemsP = do
  ia <- itemA
  ib <- itemBC B
  ic <- itemBC C
  id <- itemD
  return [ia,ib,ic,id]

itemBody :: Parser () -> Parser (Bool, String)
itemBody p = do
  correct <- itemCorrect
  itemWords <- manyTill' word p
  return (correct, unwords itemWords)

itemLetter :: Letter -> Parser ()
itemLetter l = do
  string (show l)
  choice [char ')', char ':']
  return ()

itemCorrect :: Parser Bool
itemCorrect = do
  correct <- option [] $ string "CORRECT)"
  return (notNull correct)

itemA :: Parser Item
itemA = do
  itemLetter A
  (correct, itemWords) <- itemBody $ itemLetter B
  return Item {letter = A, correct = correct, text = itemWords}

itemBC :: Letter -> Parser Item
itemBC l = do
  (correct, itemWords) <- itemBody $ itemLetter (succ l)
  return Item {letter = l, correct = correct, text = itemWords}

itemD :: Parser Item
itemD = do
  (correct, itemWords) <- itemBody $ ((string "---") >> return ())
  return Item {letter = D, correct = correct, text = itemWords}

--
-- main

main :: IO ()
main = do
  [filename] <- getArgs
  result <- parseFromFile examQuestions filename
  let yearEditionExt = splitOn "-" filename
      (year, editionExt) = (yearEditionExt !! 0, yearEditionExt !! 1)
      edition = head $ splitOn "." editionExt
  case result of
    Left err -> print err
    Right qs ->
      BL.putStr
        (encodePretty $
         Exam
         {year = (read year :: Int), edition = edition, questions = qs})
