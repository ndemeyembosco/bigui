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
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module SDzipper
    (SimpleDiagram (..), SDCtx (..), SDzipper, upZ, leftZ, rightZ,
    upmostZ, editZ, unZipSD, unZipWith, makeZipper, downZ) where

import Diagrams.Prelude


type Sides = Int


data SimpleDiagram where
  P       :: Primitive      -> SimpleDiagram
  Atop    :: SimpleDiagram  -> SimpleDiagram  -> SimpleDiagram
  T       :: Transformation -> SimpleDiagram  -> SimpleDiagram
  Iterate :: Int            -> Transformation -> SimpleDiagram -> SimpleDiagram
  deriving (Show, Eq)

data Primitive where
  Circle   :: Primitive
  Triangle :: Primitive
  Square   :: Primitive 
  Polygon  :: Sides -> Primitive
  deriving (Show, Eq)


data Transformation where
  Scale     :: Double    -> Transformation
  Translate :: V2 Double -> Transformation
  deriving (Show, Eq)





-- data SimpleDiagram where
--   SEmpty    :: SimpleDiagram
--   Circle    :: SimpleDiagram
--   Square    :: SimpleDiagram
--   Triangle  :: SimpleDiagram
--   Polygon   :: Sides         -> SimpleDiagram
--   Cursor    :: SimpleDiagram -> SimpleDiagram
--   Scale     :: Double        -> SimpleDiagram -> SimpleDiagram
--   Translate :: V2 Double     -> SimpleDiagram -> SimpleDiagram
--   Atop      :: SimpleDiagram -> SimpleDiagram -> SimpleDiagram
--   deriving (Show, Eq)



data SDCtx where
  Top       :: SDCtx
  ScaleCtx  :: Double         -> SDCtx          -> SDCtx
  TransCtx  :: V2 Double      -> SDCtx          -> SDCtx
  AtopLCtx  :: SDCtx          -> SimpleDiagram  -> SDCtx
  AtopRCtx  :: SimpleDiagram  -> SDCtx          -> SDCtx
  deriving (Show)


type SDzipper = (SimpleDiagram, SDCtx, T2 Double)  -- add transformations and make this a triple?

-- instance Show SDzipper where
--   show (sd, ctx, tr) = "(" ++ show sd ++ "," ++ show ctx ++ ")"

upZ :: SDzipper -> SDzipper
upZ c@(sd, Top, tr)            = c
upZ (sd, ScaleCtx d ctx, tr)   = (Scale d sd, ctx, tr <> inv (scaling d))
upZ (sd, TransCtx v ctx, tr)   = (Translate v sd, ctx, tr <> inv (translation v))
upZ (sd, AtopLCtx ctx sd1, tr) = (Atop sd sd1, ctx, tr)
upZ (sd, AtopRCtx sd1 ctx, tr) = (Atop sd1 sd, ctx, tr)

topZ :: SimpleDiagram -> SDzipper     -- a.k.a makeZipper
topZ sd = (sd, Top, mempty)

makeZipper :: SimpleDiagram -> SDzipper
makeZipper = topZ


rightZ :: SDzipper -> SDzipper
rightZ (sd, AtopLCtx ctx sd1, tr) =  (sd1, AtopRCtx sd ctx, tr)
rightZ loc                        = loc


leftZ :: SDzipper -> SDzipper
leftZ (sd, AtopRCtx sd1 ctx, tr) = (sd1, AtopLCtx ctx sd, tr)
leftZ loc                        = loc


downZ :: SDzipper -> SDzipper
downZ (Atop l r, ctx, tr)        = (l, AtopLCtx ctx r, tr)   -- by default go left first.
downZ (Scale d sd, ctx, tr)      = (sd, ScaleCtx d ctx, tr <> scaling d)
downZ (Translate v sd, ctx, tr)  = (sd, TransCtx v ctx, tr <> translation v)
downZ loc                        = loc


upmostZ :: SDzipper -> SDzipper
upmostZ l@(t, Top, tr) = l
upmostZ l              = upmostZ $ upZ l


editZ :: (SimpleDiagram -> SimpleDiagram) -> SDzipper -> SDzipper
editZ f (sd, ctx, tr) = (f sd, ctx, tr)

unZipSD :: SDzipper -> SimpleDiagram
unZipSD loc = case upmostZ loc of
  (f, s, t) -> f


unZipWith :: (SimpleDiagram -> SimpleDiagram) -> SDzipper -> SimpleDiagram
unZipWith f loc = unZipSD $ editZ f loc
