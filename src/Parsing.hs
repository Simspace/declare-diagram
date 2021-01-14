module Parsing where

import Control.Monad (void)
import Data.Either (partitionEithers)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

type Label = String

data Property
  = Class String
  | Arrow String (Maybe String) (Maybe String)
  deriving (Show)

data Tree = Node Label [Property] [Tree]
  deriving (Show)

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space (void spaceChar) lineComment empty

sc :: Parser ()
sc = L.space (void $ oneOf (" \t" :: String)) lineComment empty

symbol :: Tokens String -> Parser (Tokens String)
symbol = L.symbol (void $ oneOf (" \t" :: String))

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

pProperty :: Parser Property
pProperty = pArr
        <|> (Class <$> (symbol "class" >> pItem))
  where
  pArr :: Parser Property
  pArr = L.indentBlock scn do
    void $ symbol "arrow"
    targ <- pItem
    pure $ L.IndentMany Nothing (pure . fulfill targ) pprop
  pprop = (,) <$> lexeme (some alphaNumChar) <*> lexeme pItem
  fulfill t items =
    Arrow t (lookup "label" items) (lookup "class" items)

pComplexItem :: Parser Tree
pComplexItem = L.indentBlock scn do
  header <- pItem
  pure $ L.IndentMany Nothing (pure . xform header) pNodeMember

pNodeMember :: Parser (Either Property Tree)
pNodeMember = (Left <$> pProperty) <|> (Right <$> pComplexItem)

xform :: Label -> [Either Property Tree] -> Tree
xform h es = Node h ps ts
  where
  (ps,ts) = partitionEithers es

pItem :: Parser String
pItem = lexeme $ some (noneOf ("\n" :: String))

pTop :: Parser Tree
pTop = L.nonIndented scn . L.indentBlock scn $ do
  pure (L.IndentSome Nothing (pure . xform "Main") pNodeMember)

parser :: Parser Tree
parser = pComplexItem <* eof

parseQuoted :: Parser String
parseQuoted = do
  void $ char '"'
  strings <- many $ noneOf ("\\\"" :: String)
  void $ char '"'
  pure strings
