module Main where

import Control.Monad (when)
import Data.ByteString.Lazy (toStrict)
import Data.Foldable (traverse_)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String (fromString)
import NeatInterpolation (text)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Casing (kebab)
import Text.Megaparsec
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Parsing

treeHtml :: Tree -> H.Html
treeHtml (Node labl props subs) =
  H.div H.! A.id (fromString $ kebab labl)
        H.! A.class_ (fromString $ intercalate " " ("node" : classes))
    $ do
    H.p $ H.text (T.pack labl)
    when (not . null $ subs) $
      H.ul $
        traverse_ (H.li . treeHtml) subs

  where
  classes = flip mapMaybe props \case
    Class c -> Just c
    _ -> Nothing

arrows :: Tree -> [(T.Text, T.Text, T.Text, T.Text)]
arrows (Node labl props subs) = arrs ++ concatMap arrows subs
  where
  arrs = flip mapMaybe props \case
    Arrow target lab klass ->
      Just ( T.pack $ kebab labl
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
mkSVGEl = function (tag, attrs) {
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

  var g    = mkSVGEl("g", { class: klass });
  var line = mkSVGEl("polyline", { points: linepoints });
  var tri  = mkSVGEl("polygon", { points: arrowpoints });
  var txt  = mkSVGEl("text", {x: 200, y: 40});
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
  conns = T.concat $ map (\(n, (orig, targ, labl, klass)) ->
    let ns = T.pack (show n)
    in [text| drawConnector(${ns}, "#${orig}", "#${targ}", "${labl}", "${klass}"); |]) (zip ([0..] :: [Int]) (arrows tree))

main :: IO ()
main = do
  runParser parser "" <$> getContents >>= \case
    Right tree -> do
      let html = renderHtml . H.docTypeHtml . pageHtml $ tree
      B.writeFile "out.html" (toStrict html)
    Left e -> do
      putStr $ show e
