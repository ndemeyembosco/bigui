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
import Data.Zip as Z



import Graphics.Svg.Core
import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.Core as T
-- import qualified Diagrams.TwoD.Apollonian as DA (center, radius)

import Data.Functor
import Data.List
import qualified Data.Text.Internal as LT
import qualified Data.Text.Internal.Lazy as LTZ
import qualified Data.Text.Lazy as DT

-- import
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



type Sides = Int

data SimpleDiagram where
  SEmpty     :: SimpleDiagram
  Circle    :: SimpleDiagram
  Square    :: SimpleDiagram
  Triangle  :: SimpleDiagram
  Polygon   :: Sides            -> SimpleDiagram
  Scale     :: Double           -> SimpleDiagram -> SimpleDiagram
  Translate :: V2 Double        -> SimpleDiagram -> SimpleDiagram
  Atop      :: SimpleDiagram    -> SimpleDiagram -> SimpleDiagram
  deriving (Show)

data CHOICE = LEFT | RIGHT
    deriving (Show, Eq)

type PATH = [CHOICE]

interpSimpleDiagram' :: PATH -> SimpleDiagram -> QDiagram SVG V2 Double [PATH]
interpSimpleDiagram' p SEmpty                 = mempty
interpSimpleDiagram' p  Circle                = circle 1 # value [p]
interpSimpleDiagram' p Square                 = square 1 # value [p]
interpSimpleDiagram' p Triangle               = triangle 1 # value [p]
interpSimpleDiagram' p (Polygon sds)          = regPoly sds 1 # value [p]
interpSimpleDiagram' p (Scale d sd)           = interpSimpleDiagram' p sd # scale d
interpSimpleDiagram' p (Translate c d)        = interpSimpleDiagram' p  d # translate c
interpSimpleDiagram' p (Atop d1 d2)           = interpSimpleDiagram' (p ++ [LEFT]) d1 `atop` interpSimpleDiagram' (p ++ [RIGHT]) d2


interpSD :: SimpleDiagram -> QDiagram SVG V2 Double [PATH]
interpSD sd = interpSimpleDiagram' [] sd


example1 :: SimpleDiagram
example1 = Atop Circle (Translate (r2 (1.0, 1.0)) (Scale 2.0 (Atop Circle (Translate (r2 (1.0, 1.0)) Circle))))

renderSimpleDiagram' :: SimpleDiagram -> QDiagram SVG V2 Double [PATH]
renderSimpleDiagram' = interpSD



myDiag' :: QDiagram SVG V2 Double [PATH]
myDiag' = renderSimpleDiagram' example1

-------------- Dealing with the SVG -------------------

parseSVG :: String -> String
parseSVG [] = []
parseSVG s@(x:xs)
             |take 4 s == "<svg" = s
             |otherwise =  parseSVG xs



diagTuple' :: QDiagram SVG V2 Double [PATH] -> (T2 Double, Element)
diagTuple' myDiag' = renderDiaT SVG (SVGOptions (mkWidth 250) Nothing "" [] True) myDiag'

renderedString' :: QDiagram SVG V2 Double [PATH] -> (T2 Double, String)
renderedString' myDiag = case diagTuple' myDiag of
  (t, rendered) -> (t, DT.unpack $ renderText $ rendered)

------------ Compiling/ Parsing  ---------------------------------------------

lexer :: TokenParser u
lexer = makeTokenParser $
  emptyDef
   { reservedNames   = ["circle", "scale", "translate", "atop", "(", ",", ")"]}

myparens :: Parser a -> Parser a
myparens = parens lexer

myreserved, myreservedOp :: String -> Parser ()
myreserved   = reserved lexer
myreservedOp = reservedOp lexer

mysymbol :: String -> Parser String
mysymbol = symbol lexer

ident :: Parser String
ident = identifier lexer

mydouble :: Parser Double
mydouble = float lexer

myinteger :: Parser Sides
myinteger = fromIntegral <$> integer lexer

mywhiteSpace :: Parser ()
mywhiteSpace = whiteSpace lexer

myparse :: Parser a -> String -> Either P.ParseError a
myparse p = P.parse p ""

parseCoord :: Parser (V2 Double)
parseCoord = r2 <$> ((,) <$> (mysymbol "(" *> mydouble <* mysymbol ",") <*> (mydouble <* mysymbol ")"))

parseAtom :: Parser SimpleDiagram
parseAtom = Circle <$ myreserved "circle"
    <|> Triangle  <$ myreserved "triangle"
    <|> Square    <$ myreserved "square"
    <|> Polygon  <$> (myreserved "polygon" *> myinteger)
    <|> Scale <$> (myreserved "scale" *> mydouble) <*> parseAtom
    <|> Translate <$> (myreserved "translate" *> parseCoord) <*> parseAtom
    <|> Atop <$> (myreserved "atop" *> parseAtom) <*> parseAtom
    <|> myparens parseAtom


evalExpr' :: String -> Maybe (QDiagram SVG V2 Double [PATH])
evalExpr' s = case myparse parseAtom s of
  Right sd -> Just (interpSD sd)
  Left _   -> Nothing

isCompileReady :: String -> Bool
isCompileReady s = case evalExpr' s of
  Nothing -> False
  Just _  -> True


-------------------------------------------- GUI -------------------------------------------------------------------------


data SDdata where
  FrmCode   :: String -> SDdata
  DragCoord :: V2 Double -> [PATH] -> SDdata
  Click     :: P2 Double -> SDdata
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

      -- transform point into diagram coordinates and generate paths events from it

      mousedownTrE        = transform <$> (inv <$> transformB) T.<@> (makePoint <$> mousedownE)
      pathE               = sample    <$> q2DiagramB           T.<@> mousedownTrE

      -- to avoid lagging zip the point and path obtained from it into one event
      (pointPathTupleE :: T.Event (P2 Double, [PATH]))                = zipE mousedownTrE pathE
      mouseOutTupleE                                                  = T.filterE (\(p, path) -> path == []) pointPathTupleE

      -- helper events for what it means to be dragging
      (ismousedownE :: T.Event Bool)                                  = (/= []) <$> pathE
      (ismouseUpE   :: T.Event Bool)                                  = False <$ mouseUpE
      (ismouseDragE :: T.Event Bool)                                  = T.unionWith (&&) ismousedownE ismouseUpE

      -- again to avoid lag point and drag flag associated with it into one event

      (pointMoveBoolE :: T.Event (P2 Double, Bool))                   = zipE (makePoint <$> mousemoveE) ismouseDragE

  ismouseDragB                                   <- T.stepper False ismouseDragE
  pathB                                          <- T.stepper [] pathE
  mousedownTrB                                   <- T.stepper (makePoint (0, 0)) mousedownTrE


  let

   -- untransformed and transformed dragging events

      draggingE        = T.whenE ismouseDragB mousemoveE
      mvPointsE        = transform <$> (inv <$> transformB) T.<@> (makePoint <$> draggingE)

    -- merge mousedown event with drag event to avoid jumping when dragging resumes

      unionEventMvDown = T.unionWith const mousedownTrE mvPointsE
      mouseOutE        = fst <$> mouseOutTupleE

  bPrevMousePt                                     <- T.stepper origin unionEventMvDown

  let

  -- simulate dragging
      (translations       :: T.Event (V2 Double))                     = fmap (\prev cur -> cur .-. prev) bPrevMousePt T.<@> mvPointsE  -- to modify to incorporate rescaling

  {- merge all possible edits to the diagram into one data type
  , run the edits and generate behavior of obtained simpleDiagram -}

      (formedSDdataE  :: T.Event SDdata)                              = mergeEvents codeAreaE translations mouseOutE pathB
      (simpleDiagramE :: T.Event (SimpleDiagram -> SimpleDiagram))    = T.filterJust (runSDdata <$> formedSDdataE)

  (simpleDiagramB  :: T.Behavior SimpleDiagram)     <- T.accumB SEmpty simpleDiagramE

  let
  -- render diagram

      dTupleB       = makeDToDraw <$> simpleDiagramB
      diagramStrB   = fmap (\(x, y, z) -> x) dTupleB
      transformB    = fmap (\(x, y, z) -> y) dTupleB
      q2DiagramB    = fmap (\(x, y, z) -> z) dTupleB
      debuggStrB    = (show <$> mousedownTrB)
      codeAreaStrB  = (pprintTree <$> simpleDiagramB)


  -- Sink diagram behavior into appropriate gui elements

  T.element debuggArea # T.sink UI.text debuggStrB
  T.element diagWindow # T.sink UI.html diagramStrB
  T.element codeArea   # T.sink UI.value codeAreaStrB

  return ()

-- helper functions


zipE :: T.Event a -> T.Event b -> T.Event (a, b)
zipE e1 e2 = fmap (\(Just s, Just t) -> (s, t)) (T.unionWith (\(Just p, Nothing) (Nothing, Just pth) -> (Just p, Just pth)) (fmap (\p -> (Just p, Nothing)) e1)
                                                       (fmap (\pth -> (Nothing, Just pth)) e2))

runSDdata :: SDdata -> Maybe (SimpleDiagram -> SimpleDiagram)
runSDdata (FrmCode str) = case myparse parseAtom str of
  Right sd -> Just $ const sd
  Left  _  -> Nothing
runSDdata (DragCoord cd pths) = Just $ \sd -> foldr (\pth -> refactorTree  pth cd) sd pths
runSDdata (Click pt) = Just $ \sd -> Atop (createNewCircle pt) sd


mergeEvents :: T.Event String -> T.Event (V2 Double) -> T.Event (P2 Double) -> T.Behavior [PATH] -> T.Event SDdata
mergeEvents e1 e2 e3 bpths = head <$> T.unions [(FrmCode <$> e1), (flip DragCoord <$> bpths T.<@> e2), Click <$> e3]

makePoint :: (Int, Int) -> P2 Double
makePoint (x, y) = p2 (fromIntegral x, fromIntegral y)

refactorTree :: PATH -> V2 Double -> SimpleDiagram -> SimpleDiagram
refactorTree  _ p Circle                      = Translate p Circle
refactorTree  _ p Triangle                    = Translate p Triangle
refactorTree  _ p Square                      = Translate p Square
refactorTree  _ p (Polygon n)                 = Translate p (Polygon n)
refactorTree  pth (V2 d1 d2) (Scale d sd)     = Scale d (refactorTree  pth (V2 (d1 / d) (d2 / d)) sd)
refactorTree  pth p (Translate t sd)          = case sd of
  Circle      -> Translate (p ^+^ t) sd
  Triangle    -> Translate (p ^+^ t) sd
  Square      -> Translate (p ^+^ t) sd
  Polygon _   -> Translate (p ^+^ t) sd
  _           -> Translate t (refactorTree  pth p sd)
refactorTree  pth p  d@(Atop sd1 sd2)         = case pth of
  (x:xs)      -> case x of
    LEFT      -> Atop (refactorTree  xs p sd1) sd2
    RIGHT     -> Atop sd1 (refactorTree  xs p sd2)


pprintVec :: V2 Double -> String
pprintVec (V2 d1 d2) = "(" ++ printf "%.3f" d1 ++ "," ++ printf "%.3f" d2 ++ ")"

pprintTree :: SimpleDiagram -> String
pprintTree SEmpty                      = ""
pprintTree Circle                      = "circle"
pprintTree Square                      = "square"
pprintTree Triangle                    = "triangle"
pprintTree (Polygon n)                 = "polygon " ++ show n
pprintTree (Scale d sd)                = "scale " ++ printf "%.3f" d ++ " (" ++ (pprintTree sd) ++ ")"
pprintTree (Translate v@(V2 d1 d2) sd) = "translate " ++ pprintVec v ++ " (" ++ (pprintTree sd) ++ ")"
pprintTree (Atop sd1 sd2)              = "atop" ++ " (" ++ (pprintTree sd1) ++ ") " ++ " (" ++ (pprintTree sd2) ++ ")"



-------------------------------------------------- scaling    ---------------------------------------------------------

centerDiff :: P2 Double  -> P2 Double -> PATH -> SimpleDiagram -> Double
centerDiff pt cntr pths SEmpty                            = 0.0
centerDiff pt cntr pths Circle                            = norm (pt .-. cntr)
centerDiff pt cntr pths (Translate v@(V2 d1 d2) sd)       = centerDiff pt (p2 (d1, d2)) pths sd
centerDiff pt cntr pths (Scale d sd)                      = (centerDiff pt cntr pths sd) / d -- keep it constrained in (0,1) range
centerDiff pt cntr pths (Atop sd1 sd2)                    = case pths of
    (x:xs)      -> case x of
      LEFT      -> centerDiff pt cntr xs sd1
      RIGHT     -> centerDiff pt cntr xs sd2
centerDiff pt cntr pths _                                 = undefined

isDragBounded :: P2 Double -> P2 Double -> PATH -> SimpleDiagram -> Bool
isDragBounded pt cntr pths sd = centerDiff pt cntr pths sd <= 0.7


reScale :: SimpleDiagram -> P2 Double -> P2 Double -> PATH -> SimpleDiagram
reScale SEmpty _ _      pth                              = SEmpty
reScale Circle prevP newP pth                            = Scale (norm $ newP .-. prevP) Circle
reScale (Translate v@(V2 d1 d2) sd) prevP newP  pth      = Translate v (reScale sd prevP newP pth)
reScale (Scale d sd)                prevP newP  pth      = Scale d (reScale sd prevP newP pth)
reScale sd@(Atop sd1 sd2)           prevP newP  pth      = case pth of
  (x:xs)  -> case x of
    LEFT  -> Atop (reScale sd1 prevP newP xs) sd2
    RIGHT -> Atop sd1 (reScale sd2 prevP newP xs)
  otherwise -> sd


-------------------------------------------------------------------------------------------------------------------

createNewCircle :: P2 Double -> SimpleDiagram
createNewCircle p = Translate (p .-. origin) Circle

makeDToDraw :: SimpleDiagram -> (String, (T2 Double), (QDiagram SVG V2 Double [PATH]))
makeDToDraw sd = let code = interpSD sd # withEnvelope (square 4 :: Diagram B) in let (tr, svgStr) = renderedString' code in (parseSVG $ svgStr, tr, code)
