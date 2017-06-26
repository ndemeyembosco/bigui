{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}


import Control.Monad
import qualified Control.Applicative as A



import Graphics.Svg.Core
import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.Core as T

import Data.Functor
import Data.List
import qualified Data.Text.Internal as LT
import qualified Data.Text.Internal.Lazy as LTZ
import qualified Data.Text.Lazy as DT
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Query

import qualified Text.Parsec            as P
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Expr
import           Text.Parsec.Language
import           Text.Parsec.Pos
import           Text.Parsec.Prim
import           Text.Printf

import           Text.Parsec.String
import           Text.Parsec.Token
import           Data.Maybe
import           Data.IORef
import           Control.Monad.IO.Class
import           SDzipper
import           MyParser
import           KeyCode

{- ------------- set all other parts of diagram to false execpt those under the cursor, which
will also be rendered thicker than others. ---------------------- -}

interpSimpleDiagram :: SimpleDiagram -> QDiagram SVG V2 Double [Bool]
interpSimpleDiagram SEmpty           = mempty
interpSimpleDiagram Circle           = circle 1 # value [False]
interpSimpleDiagram Square           = square 1 # value [False]
interpSimpleDiagram Triangle         = triangle 1 # value [False]
interpSimpleDiagram (Polygon sds)    = regPoly sds 1 # value [False]
interpSimpleDiagram (Cursor sd)      = [True] <$ (interpSimpleDiagram sd  # lw veryThick)
interpSimpleDiagram (Scale d sd)     = interpSimpleDiagram sd # scale d
interpSimpleDiagram (Translate v sd) = interpSimpleDiagram sd # translate v
interpSimpleDiagram (Atop sd1 sd2)   = interpSimpleDiagram sd1 `atop` interpSimpleDiagram sd2

------------------------------------------------------------------------------------------------
parseSVG :: String -> String
parseSVG [] = []
parseSVG s@(x:xs)
             |take 4 s == "<svg" = s
             |otherwise =  parseSVG xs


diagTuple' :: QDiagram SVG V2 Double [Bool] -> (T2 Double, Element)
diagTuple' myDiag' = renderDiaT SVG (SVGOptions (mkWidth 250) Nothing "" [] True) myDiag'

renderedString' :: QDiagram SVG V2 Double [Bool] -> (T2 Double, String)
renderedString' myDiag = case diagTuple' myDiag of
 (t, rendered) -> (t, DT.unpack $ renderText $ rendered)

-------------------------------------------------------------------------------------------------
parseCoord :: Parser (V2 Double)
parseCoord = r2 <$> ((,) <$> (mysymbol "(" *> mydouble <* mysymbol ",") <*> (mydouble <* mysymbol ")"))

parseCursor :: Parser SimpleDiagram
parseCursor = Cursor <$> mybrackets parseAtom

parseAtom :: Parser SimpleDiagram
parseAtom = Circle <$ myreserved "circle"
    <|> Triangle  <$ myreserved "triangle"
    <|> Square    <$ myreserved "square"
    <|> Polygon  <$> (myreserved "polygon" *> myinteger)
    <|> Scale <$> (myreserved "scale" *> mydouble) <*> parseAtom
    <|> Translate <$> (myreserved "translate" *> parseCoord) <*> parseAtom
    <|> Atop <$> (myreserved "atop" *> parseAtom) <*> parseAtom
    <|> myparens parseAtom
    <|> parseCursor


evalExpr' :: String -> Maybe (QDiagram SVG V2 Double [Bool])
evalExpr' s = case myparse parseAtom s of
  Right sd -> Just (interpSimpleDiagram sd)
  Left _   -> Nothing

isCompileReady :: String -> Bool
isCompileReady s = case evalExpr' s of
  Nothing -> False
  Just _  -> True

---------------------------------------------------------------------------------------

data SDdata where
  FrmCode   :: String   -> SDdata
  DragCoord :: V2 Double -> SDdata
  Click     :: P2 Double -> SDdata
  -- SCale     :: Double    -> [PATH] -> SDdata
  Nav       :: KeyCode -> SDdata
  deriving (Show)




main :: IO ()
main = do
  T.startGUI T.defaultConfig setup

setup :: T.Window -> T.UI ()
setup window = void $ mdo

  -- GUI components set up and styling
  diagWindow         <- UI.div T.#. "diagram" # T.set T.style [("width", "500px"), ("height", "600px"), ("border", "1px solid #000")]
  codeArea           <- UI.textarea T.# T.set (T.attr "rows") "50" T.# T.set (T.attr "cols") "50"
  debuggArea         <- UI.div T.#. "debugg" # T.set T.style [("width", "500px"), ("height", "600px"), ("border", "1px solid #000")]
  T.getBody window T.#+ [UI.grid [[UI.row [T.element diagWindow, T.element debuggArea], UI.row [T.element codeArea]]]]

  let

  -- Track all user input events
      codeAreaE           = UI.valueChange codeArea
      mouseUpE            = UI.mouseup diagWindow
      mousedownE          = UI.mousedown diagWindow
      mousemoveE          = UI.mousemove diagWindow
      navKeyPressE        = T.filterE (\kc -> (keyCodeLookup kc) `elem` [ArrowUp, ArrowDown, ArrowLeft, ArrowRight]) (UI.keydown codeArea)


      -- transform point into diagram coordinates and generate cursors events from it
      mousedownTrE        = transform <$> (inv <$> transformB) T.<@> (makePoint <$> mousedownE)
      cursorE               = sample    <$> q2DiagramB           T.<@> mousedownTrE

      -- to avoid lagging zip the point and cursor obtained from it into one event
      (pointPathTupleE :: T.Event (P2 Double, [Bool]))                = zipE mousedownTrE cursorE
      mouseOutTupleE                                                  = T.filterE (\(p, cursor) -> cursor == []) pointPathTupleE
      mouseInTupleE                                                   = T.filterE (\(p, cursor) -> cursor /= []) pointPathTupleE


      -- helper events for what it means to be dragging
      ismousedownE                                                    = True <$ mouseInTupleE
      (ismouseUpE   :: T.Event Bool)                                  = False <$ mouseUpE
      (ismouseDragE :: T.Event Bool)                                  = T.unionWith (&&) ismousedownE ismouseUpE

      -- again to avoid lagging, put point and drag flag associated with it into one event
      (pointMoveBoolE :: T.Event (P2 Double, Bool))                   = zipE (makePoint <$> mousemoveE) ismouseDragE

  ismouseDragB                                   <- T.stepper False ismouseDragE
  cursorB                                          <- T.stepper [] cursorE
  mousedownTrB                                   <- T.stepper (makePoint (0, 0)) mousedownTrE
  multKeyPressB                                  <- T.stepper 0 navKeyPressE

  let
   -- untransformed and transformed clicking, dragging and scaling events
      mouseOutE        = fst <$> mouseOutTupleE
      draggingE        = T.whenE ismouseDragB mousemoveE
      mvPointsE        = transform <$> (inv <$> transformB) T.<@> (makePoint <$> draggingE)
    -- merge mousedown event with drag event to avoid jumping when dragging resumes
      unionEventMvDown = T.unionWith const (fst <$> mouseInTupleE) mvPointsE

  bPrevMousePt                                     <- T.stepper origin unionEventMvDown

  let

  -- simulate dragging
      (translations       :: T.Event (V2 Double))                     = fmap (\prev cur -> cur .-. prev) bPrevMousePt T.<@> mvPointsE

  {- merge all possible edits to the diagram into one data type
  , run the edits and generate behavior of obtained simpleDiagram -}

      (formedSDdataE  :: T.Event SDdata)                              = mergeEvents codeAreaE translations mouseOutE navKeyPressE
      (simpleDiagramE :: T.Event (SDzipper -> SDzipper))    = T.filterJust (runSDdata <$> formedSDdataE)

  (simpleDiagramB'  :: T.Behavior SDzipper)     <- T.accumB (SEmpty, Top) simpleDiagramE

  let
  -- render diagram
      simpleDiagramB = unZipSD <$> simpleDiagramB'
      dTupleB       = makeDToDraw <$> simpleDiagramB
      diagramStrB   = fmap (\(x, y, z) -> x) dTupleB
      transformB    = fmap (\(x, y, z) -> y) dTupleB
      q2DiagramB    = fmap (\(x, y, z) -> z) dTupleB
      debuggStrB    = (show <$> simpleDiagramB')
      codeAreaStrB  = (pprintTree <$> simpleDiagramB)


  -- Sink diagram behavior into appropriate gui elements
  T.element debuggArea # T.sink UI.text debuggStrB
  T.element diagWindow # T.sink UI.html diagramStrB
  T.element codeArea   # T.sink UI.value codeAreaStrB

  return ()




------------------------------- helpers  ----------------------------------------


-- merge two events that happen at the same time to avoid lagging.
zipE :: T.Event a -> T.Event b -> T.Event (a, b)
zipE e1 e2 = fmap (\(Just s, Just t) -> (s, t)) (T.unionWith (\(Just p, Nothing) (Nothing, Just pth) -> (Just p, Just pth)) (fmap (\p -> (Just p, Nothing)) e1)
                                                       (fmap (\pth -> (Nothing, Just pth)) e2))

-- render syntax tree
makeDToDraw :: SimpleDiagram -> (String, (T2 Double), (QDiagram SVG V2 Double [Bool]))
makeDToDraw sd = let code = interpSimpleDiagram sd # withEnvelope (square 4 :: Diagram B) in let (tr, svgStr) = renderedString' code in (parseSVG $ svgStr, tr, code)



-- handle different kinds of edits to the diagram
runSDdata :: SDdata -> Maybe (SDzipper -> SDzipper)
runSDdata (FrmCode str)   = case myparse parseAtom str of
  Right sd -> Just $ const (makeZipper sd)
  Left  _  -> Nothing
runSDdata (DragCoord cd)  = Just $ \zp -> refactorTree cd zp
runSDdata (Click pt)      = Just $ \sd -> editZ (Atop (createNewCircle pt)) sd
runSDdata (Nav k)  = Just $ \zp -> navigateTree (keyCodeLookup k) zp


-- creating and new circle, to be used on click outside of diagram.
createNewCircle :: P2 Double -> SimpleDiagram
createNewCircle p = Translate (p .-. origin) Circle


-- merge all different kinds of edits to the diagram into one data type.
mergeEvents :: T.Event String -> T.Event (V2 Double) -> T.Event (P2 Double) -> T.Event KeyCode -> T.Event SDdata
mergeEvents e1 e2 e3 e4 = head <$> T.unions [FrmCode <$> e1, DragCoord <$> e2, Click <$> e3, Nav <$> e4]

-- turn returned coordinate into point
makePoint :: (Int, Int) -> P2 Double
makePoint (x, y) = p2 (fromIntegral x, fromIntegral y)


--- probably wrong.

refactorTree :: V2 Double -> SDzipper -> SDzipper
refactorTree v sz = editZ (refactorTree' v) sz

refactorTree' :: V2 Double -> SimpleDiagram -> SimpleDiagram
refactorTree'  p Circle                      = Translate p Circle
refactorTree'  p Triangle                    = Translate p Triangle
refactorTree'  p Square                      = Translate p Square
refactorTree'  p (Polygon n)                 = Translate p (Polygon n)
refactorTree'  p (Cursor sd)                 = Cursor (refactorTree' p sd)
refactorTree'  (V2 d1 d2) (Scale d sd)       = Scale d (refactorTree' (V2 (d1 / d) (d2 / d)) sd)
refactorTree'  p (Translate t sd)            = case sd of
  Circle      -> Translate (p ^+^ t) sd
  Triangle    -> Translate (p ^+^ t) sd
  Square      -> Translate (p ^+^ t) sd
  Polygon _   -> Translate (p ^+^ t) sd
  _           -> Translate t (refactorTree' p sd)
refactorTree' p  d@(Atop sd1 sd2)         = Atop (refactorTree' p sd1) (refactorTree' p sd2)



{- handle contro-hey presses -}
navigateTree :: Key -> SDzipper -> SDzipper
navigateTree k z
                  |k == ArrowDown  = downZ z
                  |k == ArrowUp    = upZ z
                  |k == ArrowLeft  = leftZ z
                  |k == ArrowRight = rightZ z
                  |otherwise       = z


{- pretty print the updated syntax tree
   in order to sink it in the textarea.
 -}

pprintVec :: V2 Double -> String
pprintVec (V2 d1 d2) = "(" ++ printf "%.3f" d1 ++ "," ++ printf "%.3f" d2 ++ ")"


pprintTree ::  SimpleDiagram -> String
pprintTree SEmpty                      = ""
pprintTree Circle                      = "circle"
pprintTree Square                      = "square"
pprintTree Triangle                    = "triangle"
pprintTree (Polygon n)                 = "polygon " ++ show n
pprintTree (Cursor sd)                 = "[" ++ pprintTree sd ++ "]"
pprintTree (Scale d sd)                = "scale " ++ printf "%.3f" d ++ "(" ++ (pprintTree sd) ++ ")"
pprintTree (Translate v@(V2 d1 d2) sd) = "translate" ++ pprintVec v ++ "(" ++ (pprintTree sd) ++ ")"
pprintTree (Atop sd1 sd2)              = "atop " ++ "(" ++  (pprintTree sd1) ++ ") " ++ "(" ++  (pprintTree sd2) ++ ")"

replicateSp :: Int -> String
replicateSp n = foldr (++) [] (replicate n " ")
