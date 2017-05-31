{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}



import Control.Monad



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

------------ Compiling ---------------------------------------------

-- parsing ---

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

-----------GUI ---------------------------------

main :: IO ()
main = do
  T.startGUI T.defaultConfig setup

setup :: T.Window -> T.UI ()
setup window = void $ do
  (diagWindow, codeArea, compButton) <- buildUI window
  (fstDia, trans, _)                 <- renderSDiagram SEmpty
  (ptTransformRef, diagramRef, syntaxTreeRef, dragDetected, lastMousePos, paths)  <- makeIORefs trans fstDia SEmpty False (0.0, 0.0) []

  T.on UI.click compButton $ \_ -> do
    codeString         <- T.get T.value codeArea
    let (Right cdTree) =  myparse parseAtom codeString
    drawDiagramWithName codeArea diagWindow "newDiagram" syntaxTreeRef diagramRef cdTree ptTransformRef

  T.on UI.mouseup diagWindow $ \_ -> do
    liftIO $ writeIORef dragDetected False

  T.on UI.mousemove diagWindow $ \(newXPos, newYPos) -> do

    (ptTransformer, diagramVar, syntraxTreeVar, dragFlag, (prevXpos, prevYpos), newpath) <- readFromIORefs ptTransformRef diagramRef syntaxTreeRef dragDetected lastMousePos paths
    when dragFlag $ do
        let prevPt                           = transform ptTransformer (p2 (prevXpos, prevYpos))
        let newPt                            = transform ptTransformer (p2 (fromIntegral newXPos, fromIntegral newYPos))
        let newTree                          = (foldr (\d p -> refactorTree p d (newPt .-. prevPt)) syntraxTreeVar) newpath

        drawDiagramWithName  codeArea diagWindow "newDiagram2" syntaxTreeRef diagramRef newTree ptTransformRef
        liftIO $ writeIORef lastMousePos (fromIntegral newXPos, fromIntegral newYPos)

        return ()



  T.on UI.mousedown diagWindow $ \(x, y) -> do
    liftIO $ writeIORef dragDetected True
    (ptTransformer, diagramVar, syntraxTreeVar, _, _, _) <- readFromIORefs ptTransformRef diagramRef syntaxTreeRef dragDetected lastMousePos paths

    let pt = transform ptTransformer (p2 (fromIntegral x, fromIntegral y))
    let prevPtSample                     = sample diagramVar pt

    liftIO $ writeIORef paths  prevPtSample
    liftIO $ writeIORef lastMousePos (fromIntegral x, fromIntegral y)

    when (prevPtSample == []) $ do
        let newTree = Atop (createNewCircle pt) syntraxTreeVar
        drawDiagramWithName  codeArea diagWindow "newDiagram3" syntaxTreeRef diagramRef newTree ptTransformRef
        return ()



------------- Set up Helper Methods ---------------------------------------------------------------------------------------------------------------------
buildUI :: T.Window -> T.UI (T.Element, T.Element, T.Element)
buildUI window = do
  diagWindow         <- UI.div T.#. "diagram" # T.set T.style [("width", "500px"), ("height", "600px"), ("border", "1px solid #000")]
  codeArea           <- UI.textarea T.# T.set (T.attr "rows") "50" T.# T.set (T.attr "cols") "50"
  compButton         <- UI.button # T.set T.text "Compile"
  T.getBody window T.#+ [UI.row [UI.column [T.element diagWindow],UI.column [T.element codeArea, T.element compButton]]]
  return (diagWindow, codeArea, compButton)

renderSDiagram :: SimpleDiagram -> T.UI (QDiagram SVG V2 Double [PATH], T2 Double, String)
renderSDiagram sd = do
  let fstDia         = interpSD sd # withEnvelope (square 6 :: Diagram B)
  let (trans, sdiag) = renderedString' fstDia
  return (fstDia, trans, sdiag)


refactorTree :: SimpleDiagram -> PATH -> V2 Double -> SimpleDiagram
refactorTree Circle _ p             = Translate p Circle
refactorTree Triangle _ p           = Translate p Triangle
refactorTree Square   _ p           = Translate p Square
refactorTree (Polygon n) _ p        = Translate p (Polygon n)
refactorTree (Scale d sd) pth (V2 d1 d2)     = Scale d (refactorTree sd pth (V2 (d1 / d) (d2 / d)))
refactorTree (Translate t sd) pth p = case sd of
  Circle   -> Translate (p ^+^ t) sd
  Triangle -> Translate (p ^+^ t) sd
  Square   -> Translate (p ^+^ t) sd
  Polygon _ -> Translate (p ^+^ t) sd
  _        -> Translate t (refactorTree sd pth p)
refactorTree d@(Atop sd1 sd2) pth p   = case pth of
  (x:xs)  -> case x of
    LEFT  -> Atop (refactorTree sd1 xs p) sd2
    RIGHT -> Atop sd1 (refactorTree sd2 xs p)


drawDiagramWithName :: T.Element -> T.Element -> String -> IORef (SimpleDiagram) -> IORef (QDiagram SVG V2 Double [PATH]) -> SimpleDiagram -> IORef (T2 Double)-> T.UI T.Element
drawDiagramWithName codeArea diagWindow name syntaxTreeRef diagramRef newTree ptTransformRef = do
  let newDiagram                       = (interpSD newTree) # withEnvelope (square 4 :: Diagram B)
  let (newPtTransformer, newSvgString) = renderedString' newDiagram

  liftIO $ writeIORef diagramRef newDiagram
  liftIO $ writeIORef syntaxTreeRef newTree

  T.element codeArea T.# T.set T.value (pprintTree newTree)
  diagHtmlDiv3 <- UI.div T.#. name # T.set T.html (parseSVG newSvgString)

  liftIO $ writeIORef ptTransformRef (inv newPtTransformer)
  T.element diagWindow T.# T.set T.children [diagHtmlDiv3]


makeIORefs :: T2 Double -> QDiagram SVG V2 Double [PATH] -> SimpleDiagram -> Bool -> (Double, Double) -> [PATH]
               -> T.UI (IORef (T2 Double), IORef (QDiagram SVG V2 Double [PATH]), IORef (SimpleDiagram), IORef (Bool), IORef ((Double, Double)), IORef [PATH])
makeIORefs trans fstDia sd bool coord lpths = do
  ptTransformRef         <- liftIO $ (newIORef (inv trans))
  diagramRef             <- liftIO $ (newIORef  fstDia)
  syntaxTreeRef          <- liftIO $ (newIORef sd)
  dragDetected           <- liftIO $ (newIORef bool )
  lastMousePos           <- liftIO $ (newIORef coord)
  paths                  <- liftIO $ (newIORef lpths)
  return (ptTransformRef, diagramRef, syntaxTreeRef, dragDetected, lastMousePos, paths)

readFromIORefs :: IORef (T2 Double) -> IORef (QDiagram SVG V2 Double [PATH]) -> IORef SimpleDiagram -> IORef Bool -> IORef (Double, Double) -> IORef [PATH]
          -> T.UI (T2 Double, QDiagram SVG V2 Double [PATH], SimpleDiagram, Bool, (Double, Double), [PATH])
readFromIORefs ptTransformRef diagramRef syntaxTreeRef dragDetected lastMousePos paths = do
  ptTransformer                   <-  liftIO  $ readIORef ptTransformRef
  diagramVar                      <-  liftIO  $ readIORef diagramRef
  syntraxTreeVar                  <-  liftIO  $ readIORef syntaxTreeRef
  (prevXpos, prevYpos)            <-  liftIO  $ readIORef lastMousePos
  dragFlag                        <-  liftIO  $ readIORef dragDetected
  newpath                         <-  liftIO  $ readIORef paths
  return (ptTransformer, diagramVar, syntraxTreeVar, dragFlag, (prevXpos, prevYpos), newpath)


pprintVec :: V2 Double -> String
pprintVec (V2 d1 d2) = "(" ++ printf "%.3f" d1 ++ "," ++ printf "%.3f" d2 ++ ")"



pprintTree :: SimpleDiagram -> String
pprintTree SEmpty                      = "empty"
pprintTree Circle                      = "circle"
pprintTree Square                      = "square"
pprintTree Triangle                    = "triangle"
pprintTree (Polygon n)                 = "polygon " ++ show n
pprintTree (Scale d sd)                = "scale " ++ printf "%.3f" d ++ " (" ++ (pprintTree sd) ++ ")"
pprintTree (Translate v@(V2 d1 d2) sd) = "translate " ++ pprintVec v ++ " (" ++ (pprintTree sd) ++ ")"
pprintTree (Atop sd1 sd2)              = "atop" ++ " (" ++ (pprintTree sd1) ++ ") " ++ " (" ++ (pprintTree sd2) ++ ")"

createNewCircle :: P2 Double -> SimpleDiagram
createNewCircle p = Translate (p .-. origin) Circle
