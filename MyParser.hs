module MyParser
  ( -- * Lexing
   myparse, myparens, myreserved, myreservedOp,
   mysymbol, ident, mydouble, myinteger, mywhiteSpace

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

myinteger :: Parser Int
myinteger = fromIntegral <$> integer lexer

mywhiteSpace :: Parser ()
mywhiteSpace = whiteSpace lexer

myparse :: Parser a -> String -> Either P.ParseError a
myparse p = P.parse p ""
