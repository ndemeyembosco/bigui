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



type Sides = Int

-- instance A.Alternative T.Behavior where
--   empty = mempty
--   (<|>) b1 b2 = if b1 == mempty then b2 else b1

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

------------ Compiling/ compiling ---------------------------------------------

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

zipE :: T.Event a -> T.Event b -> T.Event (a, b)
zipE e1 e2 = fmap (\(Just s, Just t) -> (s, t)) (T.unionWith (\(Just p, Nothing) (Nothing, Just pth) -> (Just p, Just pth)) (fmap (\p -> (Just p, Nothing)) e1)
                                                       (fmap (\pth -> (Nothing, Just pth)) e2))

setup :: T.Window -> T.UI ()
setup window = void $ mdo
  diagWindow         <- UI.div T.#. "diagram" # T.set T.style [("width", "500px"), ("height", "600px"), ("border", "1px solid #000")]
  codeArea           <- UI.textarea T.# T.set (T.attr "rows") "50" T.# T.set (T.attr "cols") "50"
  debuggArea         <- UI.div T.#. "debugg" # T.set T.style [("width", "500px"), ("height", "600px"), ("border", "1px solid #000")]
  T.getBody window T.#+ [UI.grid [[UI.row [T.element diagWindow, T.element debuggArea], UI.row [T.element codeArea]]]]

  let
      codeAreaE           = UI.valueChange codeArea
      mouseUpE            = UI.mouseup diagWindow
      mousedownE          = UI.mousedown diagWindow
      mousemoveE          = UI.mousemove diagWindow
      mousedownTrE        = transform <$> (inv <$> transformB) T.<@> (makePoint <$> mousedownE)
      pathE               = sample    <$> q2DiagramB           T.<@> mousedownTrE

      (pointPathTupleE :: T.Event (P2 Double, [PATH]))                = zipE mousedownTrE pathE
      mouseOutTupleE                                                  = T.filterE (\(p, path) -> path == []) pointPathTupleE
      (ismousedownE :: T.Event Bool)                                  = (/= []) <$> pathE
      (ismouseUpE   :: T.Event Bool)                                  = False <$ mouseUpE
      (ismouseDragE :: T.Event Bool)                                  = T.unionWith (&&) ismousedownE ismouseUpE
      (pointMoveBoolE :: T.Event (P2 Double, Bool))                   = zipE (makePoint <$> mousemoveE) ismouseDragE

  ismouseDragB                                   <- T.stepper False ismouseDragE
  pathB                                          <- T.stepper [] pathE


  let
      draggingE        = T.whenE ismouseDragB mousemoveE
      mvPointsE        = transform <$> (inv <$> transformB) T.<@> (makePoint <$> draggingE)
      unionEventMvDown = T.unionWith const mousedownTrE mvPointsE
      mouseOutE        = fst <$> mouseOutTupleE

  bPrevMousePt                                     <- T.stepper origin unionEventMvDown

  let
      (translations       :: T.Event (V2 Double))                     = fmap (\prev cur -> cur .-. prev) bPrevMousePt T.<@> mvPointsE
      (formedSDdataE  :: T.Event SDdata)                              = mergeEvents codeAreaE translations mouseOutE pathB
      (simpleDiagramE :: T.Event (SimpleDiagram -> SimpleDiagram))    = T.filterJust (runSDdata <$> formedSDdataE)

  (simpleDiagramB  :: T.Behavior SimpleDiagram)     <- T.accumB SEmpty simpleDiagramE

  let
      dTupleB       = makeDToDraw <$> simpleDiagramB
      diagramStrB   = fmap (\(x, y, z) -> x) dTupleB
      transformB    = fmap (\(x, y, z) -> y) dTupleB
      q2DiagramB    = fmap (\(x, y, z) -> z) dTupleB
      debuggStrB    = (show <$> pathB)
      codeAreaStrB  = (pprintTree <$> simpleDiagramB)


  -- Sink
  T.element debuggArea # T.sink UI.text debuggStrB
  T.element diagWindow # T.sink UI.html diagramStrB
  T.element codeArea   # T.sink UI.value codeAreaStrB

  return ()

-- helper functions

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
refactorTree _ p Circle             = Translate p Circle
refactorTree _ p  Triangle          = Translate p Triangle
refactorTree _ p Square          = Translate p Square
refactorTree  _ p (Polygon n)        = Translate p (Polygon n)
refactorTree  pth (V2 d1 d2) (Scale d sd)     = Scale d (refactorTree  pth (V2 (d1 / d) (d2 / d)) sd)
refactorTree  pth p (Translate t sd) = case sd of
  Circle   -> Translate (p ^+^ t) sd
  Triangle -> Translate (p ^+^ t) sd
  Square   -> Translate (p ^+^ t) sd
  Polygon _ -> Translate (p ^+^ t) sd
  _        -> Translate t (refactorTree  pth p sd)
refactorTree  pth p  d@(Atop sd1 sd2) = case pth of
  (x:xs)  -> case x of
    LEFT  -> Atop (refactorTree  xs p sd1) sd2
    RIGHT -> Atop sd1 (refactorTree  xs p sd2)


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


createNewCircle :: P2 Double -> SimpleDiagram
createNewCircle p = Translate (p .-. origin) Circle

makeDToDraw :: SimpleDiagram -> (String, (T2 Double), (QDiagram SVG V2 Double [PATH]))
makeDToDraw sd = let code = interpSD sd # withEnvelope (square 4 :: Diagram B) in let (tr, svgStr) = renderedString' code in (parseSVG $ svgStr, tr, code)


-- centerDiff :: P2 Double  -> SimpleDiagram -> Double
-- centerDiff pt SEmpty = 0.0
-- centerDiff
