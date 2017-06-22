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

module SDzipper
    (SimpleDiagram (..), SDCtx (..), SDzipper, upZ, leftZ, rightZ,
    upmostZ, editZ, unZipSD, unZipWith, makeZipper) where

import Diagrams.Prelude


type Sides = Int

data SimpleDiagram where
  SEmpty    :: SimpleDiagram
  Circle    :: SimpleDiagram
  Square    :: SimpleDiagram
  Triangle  :: SimpleDiagram
  Polygon   :: Sides         -> SimpleDiagram
  Cursor    :: SimpleDiagram -> SimpleDiagram
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

makeZipper :: SimpleDiagram -> SDzipper
makeZipper = topZ


rightZ :: SDzipper -> SDzipper
rightZ (sd, AtopLCtx ctx sd1) =  (sd1, AtopRCtx sd ctx)
rightZ loc                    = loc


leftZ :: SDzipper -> SDzipper
leftZ (sd, AtopRCtx sd1 ctx) = (sd1, AtopLCtx ctx sd)
leftZ loc                    = loc


downZ :: SDzipper -> SDzipper
downZ (Atop l r, ctx)        = (l, AtopLCtx ctx r)   -- by default go left first.
downZ (Scale d sd, ctx)      = (sd, ScaleCtx d ctx)
downZ (Translate v sd, ctx)  = (sd, TransCtx v ctx)
downZ loc                    = loc


upmostZ :: SDzipper -> SDzipper
upmostZ l@(t, Top) = l
upmostZ l = upmostZ $ upZ l


editZ :: (SimpleDiagram -> SimpleDiagram) -> SDzipper -> SDzipper
editZ f (sd, ctx) = (f sd, ctx)

unZipSD :: SDzipper -> SimpleDiagram
unZipSD loc = fst $ upmostZ loc


unZipWith :: (SimpleDiagram -> SimpleDiagram) -> SDzipper -> SimpleDiagram
unZipWith f loc = unZipSD $ editZ f loc
