{-# LANGUAGE TupleSections, OverloadedStrings, LambdaCase, QuasiQuotes, BlockArguments, ScopedTypeVariables #-}

module Main where

import Control.Applicative (empty)
import Control.Monad (void, when)
import Control.Monad.Combinators (optional)
import Data.ByteString.Lazy (toStrict)
import Data.Either (partitionEithers)
import Data.Foldable (traverse_)
import Data.List (foldl', intercalate)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String (fromString)
import Data.Void (Void)
import NeatInterpolation (text)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Casing (kebab)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
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
    symbol "arrow"
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
pItem = lexeme $ some (alphaNumChar <|> char '-' <|> char ' ')

pTop :: Parser Tree
pTop = L.nonIndented scn . L.indentBlock scn $ do
  pure (L.IndentSome Nothing (pure . xform "Main") pNodeMember)

parser :: Parser Tree
parser = pComplexItem <* eof

treeHtml :: Tree -> H.Html
treeHtml (Node label props subs) =
  H.div H.! A.id (fromString $ kebab label)
        H.! A.class_ (fromString $ intercalate " " ("node" : classes))
    $ do
    H.p $ H.text (T.pack label)
    when (not . null $ subs) $
      H.ul $
        traverse_ (H.li . treeHtml) subs

  where
  classes = flip mapMaybe props \case
    Class c -> Just c
    _ -> Nothing

arrows :: Tree -> [(T.Text, T.Text, T.Text, T.Text)]
arrows (Node label props subs) = arrs ++ concatMap arrows subs
  where
  arrs = flip mapMaybe props \case
    Arrow target lab klass ->
      Just ( T.pack $ kebab label
           , T.pack $ kebab target
           , T.pack $ fromMaybe "" lab
           , T.pack $ fromMaybe "" klass
           )
    _ -> Nothing

pageHtml :: Tree -> H.Html
pageHtml tree =
  H.docTypeHtml do
    H.head do
      H.link H.! A.rel "stylesheet" H.! A.href "styles.css"
      H.script H.! A.src "https://code.jquery.com/jquery-latest.min.js" $ pure ()
      H.script do
        H.text $ [text|
function makeSVGEl(tag, attrs) {
    var el = document.createElementNS('http://www.w3.org/2000/svg', tag);
    for (var k in attrs) {
      el.setAttribute(k, attrs[k]);
    }
    return el;
}

drawConnector = function(n, a, b, label, klass) {
  var ao = $(a);
  var bo = $(b);

  var origx = ao.offset().left + ao.width();
  var origy = ao.offset().top + ao.height()/2;

  var destx = bo.offset().left;
  if (!(origx < destx)) {
    destx += bo.width();
  }
  var desty = bo.offset().top + bo.height()/2;

  var outx = origx + 35 + 5 * n;

  var trix = ((origx < destx) ? destx - 10 : destx + 6);

  var linepoints = `$${origx},$${origy} $${outx},$${origy} $${outx},$${desty} $${destx},$${desty}`;

  var arrowpoints = `$${destx},$${desty} $${trix},$${desty + 5} $${trix},$${desty - 5}`;

  var g = makeSVGEl("g", { class: klass });
  var line = makeSVGEl("polyline", { points: linepoints });
  var tri = makeSVGEl("polygon", { points: arrowpoints });
  var txt = makeSVGEl("text", {x: 200, y: 40});
  $(txt).text(label);

  g.appendChild(line);
  g.appendChild(tri);
  g.appendChild(txt);

  $(g).appendTo($("#svg"));
}

drawArrows = function() {
  ${conns}
}
        |]
    H.body H.! A.onload "drawArrows()" $ do
      svg
      treeHtml tree
  where
  svg = H.preEscapedToHtml ([text|
<svg id="svg">
</svg>
        |] :: T.Text)
  conns :: T.Text
  conns = T.concat $ map (\(n, (orig, targ, label, klass)) ->
    let ns = T.pack (show n)
    in [text| drawConnector(${ns}, "#${orig}", "#${targ}", "${label}", "${klass}"); |]) (zip [0..] (arrows tree))

main :: IO ()
main = do
  runParser parser "" <$> getContents >>= \case
    Right tree -> do
      let html = renderHtml . H.docTypeHtml . pageHtml $ tree
      B.writeFile "out.html" (toStrict html)
    Left e -> do
      putStr $ show e

parseQuoted :: Parser String
parseQuoted = do
  char '"'
  strings <- many $ noneOf ("\\\"" :: String)
  char '"'
  pure strings
