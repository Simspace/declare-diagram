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

var slots = {};

// Hand out a sequence number for a node. These 'slot' numbers can be used to calculate an offset based
// on the number of arrows going into or out of a node, or the number of arrows going through a column
// The point of the suffix is so that arrows going 'up' or 'down' can share a 0th slot
grabSlot = function(ident, suffix) {
  var k = ident + '_' + suffix;

  if (slots[ident]) {
    if (slots[k]) {
      slots[k] = slots[k] + 1;
      return slots[k];
    } else {
      slots[k] = 1;
      return slots[k];
    }
  }
  else {
    slots[ident] = true;
    slots[k] = 0;
    return slots[k];
  }
}

drawArrow = function(a, b, label, klass) {
  var ao = $('#' + a);
  var bo = $('#' + b);

  var origx = ao.offset().left + ao.width();
  var origy = ao.offset().top + ao.height()/2;

  var destx = bo.offset().left;
  if (!(origx < destx)) {
    destx += bo.width();
  }
  var desty = bo.offset().top + bo.height()/2;

  var horizOrigSlot = grabSlot(a, origy < desty ? 'down' : 'up');
  var horizDestSlot = grabSlot(b, origy < desty ? 'up' : 'down');

  var sloty = origy + horizOrigSlot * 6 * (origy < desty ? 1 : -1);
  var destSloty = desty + horizDestSlot * 6 * (origy < desty ? -1 : 1);

  var origParentCol = ao.parents('.rows').last();
  var depthOffset = origParentCol.offset().left + origParentCol.width() - origx;

  var vertSlot = grabSlot(origParentCol.attr('id'), 'singleton');

  var outx = origx + 6 * 6 - 6 * vertSlot + (origx < destx ? 40 : 24) + depthOffset;

  var trix = (origx < destx ? destx - 11 : destx + 11);

  var linepoints = `$${origx},$${sloty} $${outx},$${sloty} $${outx},$${destSloty} $${destx},$${destSloty}`;

  var arrowpoints = `$${destx},$${destSloty} $${trix},$${destSloty + 4} $${trix},$${destSloty - 4}`;

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
  var arrs = [];
  ${conns}

  // We want to draw the arrows that go to a forward column first to minimize overlap
  var forwards = []; //arrows that go to a later column
  var sames = []; //arows that stay in the same column or earlier
  arrs.forEach (function(x) {
    if ($('#' + x[0]).offset().left < $('#' + x[1]).offset().left) {
      forwards.push(x);
    } else {
      sames.push(x);
    }
  });

  forwards.forEach(function (x) {
    drawArrow(x[0], x[1], x[2], x[3]);
  });
  sames.forEach(function (x) {
    drawArrow(x[0], x[1], x[2], x[3]);
  });
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
  conns = T.concat $ map (\(orig, targ, labl, klass) ->
    [text| arrs.push(["${orig}", "${targ}", "${labl}", "${klass}"]); |]) (arrows tree)

main :: IO ()
main = do
  runParser parser "" <$> getContents >>= \case
    Right tree -> do
      let html = renderHtml . H.docTypeHtml . pageHtml $ tree
      B.writeFile "out.html" (toStrict html)
    Left e -> do
      putStr $ show e
