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
import Data.Char (isAlphaNum)

mkId :: String -> String
mkId = filter (\x -> isAlphaNum x || (x == '-'))  . kebab

treeHtml :: Tree -> H.Html
treeHtml (Node labl props subs) =
  H.div H.! A.id (fromString $ mkId labl)
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
      Just ( T.pack $ mkId labl
           , T.pack $ mkId target
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
        H.text (jsBlob tree)
    H.body H.! A.onload "drawArrows()" $ do
      svg
      treeHtml tree
  where
  svg = H.preEscapedToHtml ([text|
<svg id="svg">
</svg>
        |] :: T.Text)

main :: IO ()
main = do
  runParser parser "" <$> getContents >>= \case
    Right tree -> do
      let html = renderHtml . H.docTypeHtml . pageHtml $ tree
      B.writeFile "out.html" (toStrict html)
    Left e -> do
      putStr $ show e

jsBlob :: Tree -> T.Text
jsBlob tree =
  let conns = T.concat $ map (\(orig, targ, labl, klass) ->
                [text| arrs.push(["${orig}", "${targ}", "${labl}", "${klass}"]); |]) (arrows tree)
  in [text|
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
    connectElements(x[0], x[1], x[2], x[3]);
  });
  sames.forEach(function (x) {
    connectElements(x[0], x[1], x[2], x[3]);
  });
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

// draw an arrow from one dom element to another
// a and b are node ids
// label is a message to display on hover
// klass is a css class to add to the arrow element
connectElements = function(a, b, label, klass) {
  var ao = $('#' + a);
  var bo = $('#' + b);

  var origx = ao.offset().left + ao.width();
  var origy = ao.offset().top + ao.height()/2;

  var destx = bo.offset().left;
  if (!(origx < destx)) {
    destx += bo.width();
  }
  var desty = bo.offset().top + bo.height()/2;

  var forwardArr = origx < destx;
  var downArr = origy < desty;
  var rightArr = destx > origx;

  var horizOrigSlot = grabSlot(a, downArr ? 'down' : 'up');
  var horizDestSlot = grabSlot(b + (rightArr ? '-right' : ''), downArr ? 'up' : 'down');

  var sloty = origy + horizOrigSlot * 8 * (downArr ? 1 : -1);
  var destSloty = desty + horizDestSlot * 8 * (downArr ? -1 : 1);

  var origParentCol = ao.parents('.cols > ul > li > div').first();
  var depthOffset = origParentCol.offset().left + origParentCol.width() - origx;

  var vertSlot = grabSlot(origParentCol.attr('id'), 'singleton');

  var outx = origx + 6 * 8 - 8 * vertSlot + (forwardArr ? 40 : 24) + depthOffset;

  drawArrowf([origx,sloty], [outx,sloty], [outx,destSloty], [destx, destSloty], label, klass);
}

mkSVGEl = function (tag, attrs) {
    var el = document.createElementNS('http://www.w3.org/2000/svg', tag);
    for (var k in attrs) {
      el.setAttribute(k, attrs[k]);
    }
    return el;
}

drawArrowf = function(start, mid1, mid2, end, label, klass) {
  var trix = end[0] + (mid2[0] < end[0] ? -11 : 11);

  var g = mkSVGEl("g", { class: klass });
  // This draws the first two segments of the arrow
  var polyline = mkSVGEl("polyline", { points: `$${start[0]},$${start[1]} $${mid1[0]},$${mid1[1]} $${mid2[0]},$${mid2[1]}` });
  // This draws the last segment, which we may need to 'ghost' (i.e. lower opacity) if it's long and potentially crosses other columns
  var spanLine = mkSVGEl("line", { x1: mid2[0], y1: mid2[1], x2: end[0], y2: end[1], class: (Math.abs(end[0] - mid2[0]) > 200 ? 'ghosted' : '') });
  var tri  = mkSVGEl("polygon", { points: `$${end[0]},$${end[1]} $${trix},$${end[1] + 4} $${trix},$${end[1] - 4}` });

  var txt  = mkSVGEl("text", {x: end[0], y: end[1] - 50, 'dominant-baseline': 'hanging'});
  var rect = mkSVGEl("rect", {x: end[0] - 10, y: end[1] - 50 - 10});

  g.appendChild(polyline);
  g.appendChild(spanLine);
  g.appendChild(tri);
  var gel = $(g);

  var colors = ['#962D40', '#AF4B47', '#C57544', '#888F41', '#61754A'];
  var color = colors[Math.floor(Math.random() * colors.length)];

  gel.css('stroke', color);
  gel.children('polygon').css('fill', color);

  gel.appendTo($("#svg"));

  if (label.length > 0) {
    var textg = mkSVGEl("g", {class: 'label'} );
    $(txt).text(label);
    textg.appendChild(rect);
    textg.appendChild(txt);
    g.appendChild(textg);
    $(rect).width($(txt)[0].getBBox().width + 20);
    $(rect).height($(txt)[0].getBBox().height + 20);
  }

}
|]
