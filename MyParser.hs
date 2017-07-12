module MyParser
  ( -- * Lexing
   myparse, myparens, myreserved, myreservedOp,
   mysymbol, ident, mydouble, myinteger, mywhiteSpace, mybrackets, mysemiSep, mysemiSep1

  )
  where

import qualified Text.Parsec            as P
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Expr       hiding (Operator)
import           Text.Parsec.Language   (emptyDef)
import           Text.Parsec.Pos
import           Text.Parsec.Prim       (Consumed (..), Reply (..), State (..),
                                         runParsecT, try)
import           Text.Parsec.String     (Parser, parseFromFile)
import           Text.Parsec.Token

import           Control.Applicative
import           Data.Functor.Identity


lexer :: TokenParser u
lexer = makeTokenParser $
  emptyDef
   { reservedNames   = ["circle", "scale", "translate", "atop", "(", ",", ")"
                       , "let", "in", "=", "triangle", "square", "polygon", "iterate", "rotate"]}

myparens :: Parser a -> Parser a
myparens = parens lexer

mybrackets :: Parser a -> Parser a
mybrackets = brackets lexer

myreserved, myreservedOp :: String -> Parser ()
myreserved   = reserved lexer
myreservedOp = reservedOp lexer

mysemiSep :: Parser a -> Parser [a]
mysemiSep = semiSep lexer

mysemiSep1 :: Parser a -> Parser [a]
mysemiSep1 = semiSep1 lexer

mysymbol :: String -> Parser String
mysymbol = symbol lexer

ident :: Parser String
ident = identifier lexer

mydouble :: Parser Double
mydouble = signed $ float lexer

myinteger :: Parser Int
myinteger = fromIntegral <$> integer lexer

mywhiteSpace :: Parser ()
mywhiteSpace = whiteSpace lexer

myparse :: Parser a -> String -> Either P.ParseError a
myparse p = P.parse p ""


signed :: Num a => Parser a -> Parser a
signed pn = handleSign <$> optionMaybe (mysymbol "-") <*> pn
  where
    handleSign :: Num a => Maybe String -> a -> a
    handleSign m n = case m of
      Just t  -> negate n
      _       -> n
