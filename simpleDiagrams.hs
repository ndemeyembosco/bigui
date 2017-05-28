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
--import           Text.ParserCombinators.Parsec.Number
import           Text.Parsec.Expr
import           Text.Parsec.Language
import           Text.Parsec.Pos
import           Text.Parsec.Prim

import           Text.Parsec.String
import           Text.Parsec.Token
import           Data.Maybe
import           Data.IORef
import           Control.Monad.IO.Class





data SimpleDiagram where
  Circle    :: SimpleDiagram
  Scale     :: Double           -> SimpleDiagram -> SimpleDiagram
  Translate :: V2 Double        -> SimpleDiagram -> SimpleDiagram
  Atop      :: SimpleDiagram    -> SimpleDiagram -> SimpleDiagram
  deriving (Show)

data CHOICE = LEFT | RIGHT
    deriving (Show, Eq)

type PATH = [CHOICE]

interpSimpleDiagram' :: PATH -> SimpleDiagram -> QDiagram SVG V2 Double [PATH]
interpSimpleDiagram' p  Circle                = circle 1 # value [p]
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

-- parising ---

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

mywhiteSpace :: Parser ()
mywhiteSpace = whiteSpace lexer

myparse :: Parser a -> String -> Either P.ParseError a
myparse p = P.parse p ""

parseCoord :: Parser (V2 Double)
parseCoord = r2 <$> ((,) <$> (mysymbol "(" *> mydouble <* mysymbol ",") <*> (mydouble <* mysymbol ")"))

parseAtom :: Parser SimpleDiagram
parseAtom = Circle <$ myreserved "circle"
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

  diagWindow         <- UI.div T.#. "diagram" # T.set T.style [("width", "500px"), ("height", "600px"), ("border", "1px solid #000")]
  codeArea           <- UI.textarea T.# T.set (T.attr "rows") "50" T.# T.set (T.attr "cols") "50"
  compButton         <- UI.button # T.set T.text "Compile"
  T.getBody window T.#+ [UI.row [UI.column [T.element diagWindow],UI.column [T.element codeArea, T.element compButton]]]

  ptTransformRef         <- liftIO $ (newIORef mempty         :: IO (IORef (T2 Double)))
  diagramRef             <- liftIO $ (newIORef mempty         :: IO (IORef (QDiagram SVG V2 Double [PATH])))
  syntaxTreeRef          <- liftIO $ (newIORef Circle         :: IO (IORef (SimpleDiagram)))
  dragDetected           <- liftIO $ (newIORef False          :: IO (IORef (Bool)))
  lastMousePos           <- liftIO $ (newIORef (0.0, 0.0)     :: IO (IORef ((Double, Double))))
  paths                  <- liftIO $ (newIORef []             :: IO (IORef [PATH]))

  T.on UI.click compButton $ \_ -> do
    codeString <- T.get T.value codeArea
    T.liftIO $ print codeString

    let (Right cdTree) = myparse parseAtom codeString
    let diagram        = (fromJust $ evalExpr' codeString) # withEnvelope (square 4 :: Diagram B)
    let (t, svgString) = renderedString' diagram

    liftIO $ writeIORef ptTransformRef (inv t)
    liftIO $ writeIORef diagramRef diagram
    liftIO $ writeIORef syntaxTreeRef cdTree

    diagHtmlDiv <- UI.div T.#. "newDiagram" # T.set T.html (parseSVG svgString)
    T.element diagWindow T.# T.set T.children [diagHtmlDiv]

  T.on UI.mouseup diagWindow $ \_ -> do
    liftIO $ writeIORef dragDetected False

  T.on UI.mousemove diagWindow $ \(newXPos, newYPos) -> do

    ptTransformer                   <- liftIO $ readIORef ptTransformRef
    diagramVar                      <-  liftIO $ readIORef diagramRef
    syntraxTreeVar                  <-  liftIO $ readIORef syntaxTreeRef
    (prevXpos, prevYpos)            <- liftIO $ readIORef lastMousePos
    dragFlag                        <- liftIO $ readIORef dragDetected
    newpath                         <- liftIO $ readIORef paths

    when dragFlag $ do
        let prevPt                           = transform ptTransformer (p2 (prevXpos, prevYpos))


        let newPt                            = transform ptTransformer (p2 (fromIntegral newXPos, fromIntegral newYPos))
        let newTree                          = (foldr (\d p -> refactorTree p d (newPt .-. prevPt)) syntraxTreeVar) newpath

        let newDiagram                       = (interpSD newTree) # withEnvelope (square 4 :: Diagram B)
        let (newPtTransformer, newSvgString) = renderedString' newDiagram

        liftIO $ writeIORef diagramRef newDiagram
        liftIO $ writeIORef ptTransformRef (inv newPtTransformer)
        liftIO $ writeIORef lastMousePos (fromIntegral newXPos, fromIntegral newYPos)
        liftIO $ writeIORef syntaxTreeRef newTree

        -- liftIO $ print (newPt .-. prevPt)
        -- liftIO $ print newTree


        diagHtmlDiv2 <- UI.div T.#. "newDiagram2" # T.set T.html (parseSVG newSvgString)
        T.element diagWindow T.# T.set T.children [diagHtmlDiv2]

        return ()



  T.on UI.mousedown diagWindow $ \(x, y) -> do
    liftIO $ writeIORef dragDetected True

    ptTransformer      <- liftIO $ readIORef ptTransformRef
    diagramVar         <-  liftIO $ readIORef diagramRef
    syntraxTreeVar     <-  liftIO $ readIORef syntaxTreeRef

    let pt = transform ptTransformer (p2 (fromIntegral x, fromIntegral y))
    let prevPtSample                     = sample diagramVar pt
    liftIO $ writeIORef paths  prevPtSample
    liftIO $ writeIORef lastMousePos (fromIntegral x, fromIntegral y)
    liftIO $ print pt
    liftIO $ print $ sample diagramVar pt



refactorTree :: SimpleDiagram -> PATH -> V2 Double -> SimpleDiagram
refactorTree Circle _ p             = Translate p Circle
refactorTree (Scale d sd) pth p     = Scale d (refactorTree sd pth p)
refactorTree (Translate t sd) pth p = case sd of
  Circle -> Translate (p ^+^ t) sd
  _      -> Translate t (refactorTree sd pth p)
refactorTree d@(Atop sd1 sd2) pth p   = case pth of
  -- []      -> Translate p d
  (x:xs)  -> case x of
    LEFT  -> Atop (refactorTree sd1 xs p) sd2
    RIGHT -> Atop sd1 (refactorTree sd2 xs p)
