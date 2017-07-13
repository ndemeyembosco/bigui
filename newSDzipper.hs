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
{-# LANGUAGE MultiParamTypeClasses #-}

module NewSDzipper
    (Showzip (..), TransformationEdit (..), Primitive (..), SimpleDiagram (..), SDCtx (..), SDzipper, upZ, leftZ, rightZ,
    upmostZ, editZ, unZipSD, unZipWith, makeZipper, downZ, findTransform) where

import Diagrams.Prelude
import qualified Data.Map as M


type Sides = Int




data SimpleDiagram where
  Assign   :: String        -> SimpleDiagram -> SimpleDiagram -> SimpleDiagram
  Var      :: String        -> SimpleDiagram
  Cursor   :: SimpleDiagram -> SimpleDiagram
  Pr       :: Primitive      -> SimpleDiagram
  Atop    :: SimpleDiagram  -> SimpleDiagram  -> SimpleDiagram
  T       :: TransformationEdit -> SimpleDiagram  -> SimpleDiagram
  Iterate :: Int            -> TransformationEdit -> Maybe Int -> SimpleDiagram -> SimpleDiagram
  deriving (Show, Eq)

data Primitive where
  SEmpty   :: Primitive
  Circle   :: Primitive
  Triangle :: Primitive
  Square   :: Primitive
  Polygon  :: Sides -> Primitive
  deriving (Show, Eq)


data TransformationEdit where
  Scale     :: Double       -> TransformationEdit
  Translate :: V2 Double    -> TransformationEdit
  Rotate    :: Double       -> TransformationEdit
  deriving (Show, Eq)




data SDCtx where
  Top       :: SDCtx
  ScaleCtx  :: Double         -> SDCtx          -> SDCtx
  TransCtx  :: V2 Double      -> SDCtx          -> SDCtx
  RotateCtx :: Double         -> SDCtx          -> SDCtx
  AtopLCtx  :: SDCtx          -> SimpleDiagram  -> SDCtx
  AtopRCtx  :: SimpleDiagram  -> SDCtx          -> SDCtx
  AssignVCtx :: String         -> SDCtx         -> SimpleDiagram -> SDCtx
  AssignECtx :: String         -> SimpleDiagram -> SDCtx         -> SDCtx
  IterCtx   :: Int            -> TransformationEdit -> Maybe Int -> SDCtx -> SDCtx
  deriving (Show)



type SDzipper = (SimpleDiagram, SDCtx, T2 Double)            -- add transformations and make this a triple?

class Showzip a where
  showzip :: a -> String

instance Showzip SDzipper where
  showzip (sd, ctx, tr) = show (sd, ctx)


findTransform :: TransformationEdit -> T2 Double
findTransform (Scale d)      = scaling d
findTransform (Translate v)  = translation v
findTransform (Rotate a)     = rotation (a @@ deg)



upZ :: SDzipper -> SDzipper
upZ c@(sd, Top, tr)            = c
upZ (sd, ScaleCtx d ctx, tr)   = (T (Scale d) sd, ctx, tr <> inv (scaling d))
upZ (sd, TransCtx v ctx, tr)   = (T (Translate v) sd, ctx, tr <> inv (translation v))
upZ (sd, RotateCtx a ctx, tr)  = (T (Rotate a) sd, ctx, tr <> inv (rotation (a @@ deg)))
upZ (sd, AtopLCtx ctx sd1, tr) = (Atop sd sd1, ctx, tr)
upZ (sd, AtopRCtx sd1 ctx, tr) = (Atop sd1 sd, ctx, tr)
upZ (sd, AssignVCtx s ctx sd1, tr)  = (Assign s sd sd1, ctx, tr)
upZ (sd, AssignECtx s sd1 ctx, tr)  = (Assign s sd1 sd, ctx, tr)
upZ (sd, IterCtx n tra m ctx, tr) = (Iterate n tra m sd, ctx, tr)


topZ :: SimpleDiagram -> SDzipper     -- a.k.a makeZipper
topZ sd = (sd, Top, mempty)

makeZipper :: SimpleDiagram -> SDzipper
makeZipper = topZ


rightZ :: SDzipper -> SDzipper
rightZ (sd, AtopLCtx ctx sd1, tr)     =  (sd1, AtopRCtx sd ctx, tr)
rightZ (sd, AssignVCtx s ctx sd1, tr) =  (sd1, AssignECtx s sd ctx, tr)
rightZ loc                            = loc


leftZ :: SDzipper -> SDzipper
leftZ (sd, AtopRCtx sd1 ctx, tr)     = (sd1, AtopLCtx ctx sd, tr)
leftZ (sd, AssignECtx s sd1 ctx, tr) = (sd1, AssignVCtx s ctx sd, tr)
leftZ loc                            = loc


downZ :: SDzipper -> SDzipper
downZ (Atop l r, ctx, tr)             = (l, AtopLCtx ctx r, tr)   -- by default go left first.
downZ (Assign s sd1 sd2, ctx, tr)     = (sd1, AssignVCtx s ctx sd2, tr)
downZ (T (Scale d) sd, ctx, tr)       = (sd, ScaleCtx d ctx, tr <> scaling d)
downZ (T (Translate v) sd, ctx, tr)   = (sd, TransCtx v ctx, tr <> translation v)
downZ (T (Rotate a) sd, ctx, tr)      = (sd, RotateCtx a ctx, tr <> rotation (a @@ deg))
downZ (Iterate n tra m sd, ctx, tr)   = (sd, IterCtx n tra m ctx, tr)
downZ loc                             = loc


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
