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



renderedElement :: Diagram B -> Element
renderedElement myDiag =
  renderDia SVG (SVGOptions (mkWidth 250) Nothing "" [] True) myDiag

-- renderedText :: Diagram B -> DT.Text
-- renderedText myDiag = renderText renderedElement

renderedString :: Diagram B -> String
renderedString = DT.unpack.renderText.renderedElement

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
  diag <- UI.div T.#. "diagram"
  codeArea <- UI.textarea T.# T.set (T.attr "rows") "50" T.# T.set (T.attr "cols") "50"
  compButton <- UI.button # T.set T.text "Compile"
  T.getBody window T.#+ [UI.row [UI.column [T.element diag],UI.column [T.element codeArea, T.element compButton]]]
  T.on UI.click compButton $ \_ -> do
    frmCode <- T.get T.value codeArea
    T.liftIO $ print frmCode
    newDiagram <- UI.div T.#. "newDiagram" # T.set T.html (parseSVG $ renderedString (fromJust (evalExpr frmCode)))
    T.element diag T.# T.set T.children [newDiagram]
