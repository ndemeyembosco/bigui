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
  Translate :: (Double, Double) -> SimpleDiagram -> SimpleDiagram
  Atop      :: SimpleDiagram    -> SimpleDiagram -> SimpleDiagram
  deriving (Show)


interpSimpleDiagram :: SimpleDiagram -> Diagram B
interpSimpleDiagram Circle                 = circle 1
interpSimpleDiagram (Scale d sd)           = interpSimpleDiagram sd # scale d
interpSimpleDiagram (Translate c@(x, y) d) = interpSimpleDiagram d # translate (r2 c)
interpSimpleDiagram (Atop d1 d2)           = (interpSimpleDiagram d1) `atop` (interpSimpleDiagram d2)

example1 :: SimpleDiagram
example1 = Atop Circle (Translate (1.0, 1.0) (Scale 2.0 Circle))

renderSimpleDiagram :: SimpleDiagram -> Diagram B
renderSimpleDiagram = interpSimpleDiagram

myDiag :: Diagram B
myDiag = renderSimpleDiagram example1

-------------- Dealing with the SVG -------------------

parseSVG :: String -> String
parseSVG [] = []
parseSVG s@(x:xs)
             |take 4 s == "<svg" = s
             |otherwise =  parseSVG xs


diagTuple :: Diagram B -> (T2 Double, Element)
diagTuple myDiag = renderDiaT SVG (SVGOptions (mkWidth 250) Nothing "" [] True) myDiag

renderedElement :: Diagram B -> Element
renderedElement myDiag =
  renderDia SVG (SVGOptions (mkWidth 250) Nothing "" [] True) myDiag

-- renderedText :: Diagram B -> DT.Text
-- renderedText myDiag = renderText renderedElement

renderedString :: Diagram B -> (T2 Double, String)
renderedString myDiag = case diagTuple myDiag of
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

parseCoord :: Parser (Double, Double)
parseCoord = (,) <$> (mysymbol "(" *> mydouble <* mysymbol ",") <*> (mydouble <* mysymbol ")")


parseAtom :: Parser SimpleDiagram
parseAtom = Circle <$ myreserved "circle"
    <|> Scale <$> (myreserved "scale" *> mydouble) <*> parseAtom
    <|> Translate <$> (myreserved "translate" *> parseCoord) <*> parseAtom
    <|> Atop <$> (myreserved "atop" *> parseAtom) <*> parseAtom
    <|> myparens parseAtom


evalExpr :: String -> Maybe (Diagram B)
evalExpr s = case myparse parseAtom s of
  Right sd -> Just (interpSimpleDiagram sd)
  Left _   -> Nothing

-----------GUI ---------------------------------

main :: IO ()
main = do
  T.startGUI T.defaultConfig setup

setup :: T.Window -> T.UI ()
setup window = void $ do

  diag <- UI.div T.#. "diagram" # T.set T.style [("width", "500px"), ("height", "600px"), ("border", "1px solid #000")]
  codeArea <- UI.textarea T.# T.set (T.attr "rows") "50" T.# T.set (T.attr "cols") "50"
  compButton <- UI.button # T.set T.text "Compile"
  T.getBody window T.#+ [UI.row [UI.column [T.element diag],UI.column [T.element codeArea, T.element compButton]]]

  diaRef <- liftIO $ (newIORef mempty :: IO (IORef (T2 Double)))
  dRef   <- liftIO $ (newIORef mempty :: IO (IORef (Diagram B)))

  T.on UI.click compButton $ \_ -> do
    frmCode <- T.get T.value codeArea
    T.liftIO $ print frmCode
    let diagram = fromJust $ evalExpr frmCode
    let (t, codeStr) = renderedString diagram
    liftIO $ writeIORef diaRef (inv t)
    liftIO $ writeIORef dRef diagram

    newDiagram <- UI.div T.#. "newDiagram" # T.set T.html (parseSVG codeStr)
    T.element diag T.# T.set T.children [newDiagram]

  T.on UI.mousedown diag $ \(x, y) -> do
    diat <- liftIO $ readIORef diaRef
    dia <- liftIO $ readIORef dRef

    let pt = transform diat (p2 (fromIntegral x, fromIntegral y))
    liftIO $ print $ show pt
    liftIO $ print $ show (sample dia pt)
