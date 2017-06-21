{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module SDZipper
    (SimpleDiagram (..), SDCtx (..), SDzipper, upZ, leftZ, rightZ,
    upmostZ, editZ, unZipSD, unZipWith) where

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

data SimpleDiagram where
  SEmpty    :: SimpleDiagram
  Circle    :: SimpleDiagram
  Square    :: SimpleDiagram
  Triangle  :: SimpleDiagram
  Polygon   :: Sides         -> SimpleDiagram
  Scale     :: Double        -> SimpleDiagram -> SimpleDiagram
  Translate :: V2 Double     -> SimpleDiagram -> SimpleDiagram
  Atop      :: SimpleDiagram -> SimpleDiagram -> SimpleDiagram
  deriving (Show, Eq)



data SDCtx where
  Top       :: SDCtx
  ScaleCtx  :: Double         -> SDCtx          -> SDCtx
  TransCtx  :: V2 Double      -> SDCtx          -> SDCtx
  AtopLCtx  :: SDCtx          -> SimpleDiagram  -> SDCtx
  AtopRCtx  :: SimpleDiagram  -> SDCtx          -> SDCtx
  deriving (Show)


type SDzipper = (SimpleDiagram, SDCtx)

upZ :: SDzipper -> SDzipper
upZ c@(sd, Top)            = c
upZ (sd, ScaleCtx d ctx)   = (Scale d sd, ctx)
upZ (sd, TransCtx v ctx)   = (Translate v sd, ctx)
upZ (sd, AtopLCtx ctx sd1) = (Atop sd sd1, ctx)
upZ (sd, AtopRCtx sd1 ctx) = (Atop sd1 sd, ctx)

topZ :: SimpleDiagram -> SDzipper     -- a.k.a makeZipper
topZ sd = (sd, Top)

leftZ :: SDzipper -> SDzipper
leftZ loc = case loc of
        (Atop l r, ctx) -> (l, AtopLCtx ctx r)
        otherwise       -> loc

rightZ :: SDzipper -> SDzipper
rightZ loc = case loc of
        (Atop l r, ctx) -> (r, AtopRCtx l ctx)
        otherwise       -> loc


upmostZ :: SDzipper -> SDzipper
upmostZ l@(t, Top) = l
upmostZ l = upmostZ $ upZ l


editZ :: (SimpleDiagram -> SimpleDiagram) -> SDzipper -> SDzipper
editZ f (sd, ctx) = (f sd, ctx)

unZipSD :: SDzipper -> SimpleDiagram
unZipSD loc = fst $ upmostZ loc


unZipWith :: (SimpleDiagram -> SimpleDiagram) -> SDzipper -> SimpleDiagram
unZipWith f loc = unZipSD $ editZ f loc
