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
    (Showzip (..), TransformationEdit (..), Primitive (..), SimpleDiagram (..), SDCtx(..)
     , Assign, Prog(..), findTransform
    , GenZipper (..), LAssignGenCtx (..), ProgGenCtx (..), TransCtx (..)
    , DoubleCtx (..), IntCtx (..), VarCtx (..), AssignCtx (..)
    , upGenZ, topGenZ, makeGenZipper, downGenZ, rightGenZ, leftGenZ
    , upmostGenZ, unZipGenZ, editZipper, Modifyable (..), cursify) where

import Diagrams.Prelude
import qualified Data.Map as M


type Sides = Int

data SimpleDiagram where
  Let   :: String        -> SimpleDiagram -> SimpleDiagram -> SimpleDiagram
  Var      :: String        -> SimpleDiagram
  Cursor   :: SimpleDiagram -> SimpleDiagram
  Pr       :: Primitive      -> SimpleDiagram
  Atop    :: SimpleDiagram  -> SimpleDiagram  -> SimpleDiagram
  T       :: TransformationEdit -> SimpleDiagram  -> SimpleDiagram
  Iterate :: Int            -> TransformationEdit -> [Int] -> SimpleDiagram -> SimpleDiagram
  deriving (Show, Eq)

data Primitive where
  SEmpty   :: Primitive
  Circle   :: Primitive
  Triangle :: Primitive
  Square   :: Primitive
  Polygon  :: Sides -> Primitive
  deriving (Show, Eq)


data TransformationEdit where
  CurTr     :: TransformationEdit -> TransformationEdit
  Scale     :: Double       -> TransformationEdit
  Translate :: V2 Double    -> TransformationEdit
  Rotate    :: Double       -> TransformationEdit
  deriving (Show, Eq)




data SDCtx where
  Top          :: SDCtx
  ScaleCtx     :: Double         -> SDCtx          -> SDCtx
  TransCtx     :: V2 Double      -> SDCtx          -> SDCtx
  RotateCtx    :: Double         -> SDCtx          -> SDCtx
  AtopLCtx     :: SDCtx          -> SimpleDiagram  -> SDCtx
  AtopRCtx     :: SimpleDiagram  -> SDCtx          -> SDCtx
  LetVCtx      :: String         -> SDCtx         -> SimpleDiagram -> SDCtx
  LetECtx      :: String         -> SimpleDiagram -> SDCtx         -> SDCtx
  IterCtx      :: Int            -> TransformationEdit -> [Int] -> SDCtx -> SDCtx
  VAssignCtx   :: String        -> AssignCtx      -> SDCtx
  LAssignSDCtx :: [Assign]    -> ProgGenCtx -> SDCtx
  deriving (Show)


class Showzip a where
  showzip :: a -> String


findTransform :: TransformationEdit -> T2 Double
findTransform (Scale d)      = scaling d
findTransform (Translate v)  = translation v
findTransform (Rotate a)     = rotation (a @@ deg)
findTransform (CurTr tr)     = findTransform tr

type Assign = (String, SimpleDiagram)

data Prog where
  PVars  :: [Assign]      -> Prog
  PSdia  :: SimpleDiagram -> Prog
  ProgVS :: [Assign]      -> SimpleDiagram -> Prog
  deriving (Show)


data GenZipper where
  TopGenZ     :: GenZipper
  LAssignGenZ :: [Assign]            -> LAssignGenCtx         -> GenZipper
  AssignZ     :: Assign              -> AssignCtx  -> GenZipper
  SDZ         :: SimpleDiagram       -> SDCtx      -> GenZipper
  DoubleZ     :: Double              -> DoubleCtx  -> GenZipper
  IntZ        :: Int                 -> IntCtx     -> GenZipper
  TransZ      :: TransformationEdit  -> TransCtx   -> GenZipper
  VarZ        :: String              -> VarCtx     -> GenZipper
  deriving Show


data LAssignGenCtx where
  ProgC :: SimpleDiagram -> ProgGenCtx -> LAssignGenCtx
  deriving (Show)

data ProgGenCtx where
  TopGenCtx :: ProgGenCtx
  ProgLCtx  :: LAssignGenCtx -> SimpleDiagram -> ProgGenCtx
  ProgRCtx  :: [Assign]      -> SDCtx -> ProgGenCtx
  deriving Show

data TransCtx where
  TCtx          :: SimpleDiagram -> SDCtx -> TransCtx
  IterTransCtx  :: Int           -> [Int] -> SimpleDiagram  -> SDCtx -> TransCtx
  deriving (Show)

data DoubleCtx where
  RotDCtx    :: TransCtx -> DoubleCtx
  ScDCtx     :: TransCtx -> DoubleCtx
  TransDRCtx :: Double    -> TransCtx -> DoubleCtx
  TransDLCtx :: Double    -> TransCtx -> DoubleCtx
  deriving (Show)

data IntCtx where
  IterSIntCtx :: TransformationEdit -> [Int]              -> SimpleDiagram -> SDCtx -> IntCtx
  IterLIntCtx :: Int                -> TransformationEdit -> SimpleDiagram -> [Int] -> [Int] -> SDCtx -> IntCtx
  deriving (Show)


data VarCtx where
  VarSDCtx    :: SDCtx -> VarCtx
  VarLetCtx   :: SimpleDiagram -> SimpleDiagram -> SDCtx -> VarCtx
  VarAssCtx   :: SimpleDiagram -> AssignCtx -> VarCtx
  deriving (Show)

data AssignCtx where
  AssignCtx :: [Assign] -> [Assign] -> SimpleDiagram -> AssignCtx
  deriving (Show)

upGenZ :: (GenZipper, T2 Double) -> (GenZipper, T2 Double)
upGenZ z@(gz, tr) = case gz of
  TopGenZ            -> z
  AssignZ ass assctx -> case assctx of
    (AssignCtx [] r sd)      -> (LAssignGenZ (ass:r) (ProgC sd TopGenCtx), tr)--(SDZ sd (LAssignSDCtx (ass:r) TopGenCtx), tr)
    (AssignCtx (x:xs) r sd)  -> (AssignZ x (AssignCtx xs (ass:r) sd), tr)
    -- revise!!!!
  SDZ     sd  sdctx  -> case sdctx of
    ScaleCtx d ctx      -> (SDZ (T (Scale d) sd) ctx, tr <> inv (scaling d))
    TransCtx v ctx      -> (SDZ (T (Translate v) sd) ctx, tr <> inv (translation v))
    RotateCtx a ctx     -> (SDZ (T (Rotate a) sd) ctx, tr <> inv (rotation (a @@ deg)))
    AtopLCtx ctx sd1    -> (SDZ (Atop sd sd1) ctx, tr)
    AtopRCtx sd1 ctx    -> (SDZ (Atop sd1 sd) ctx, tr)
    LetVCtx s ctx sd1   -> (SDZ (Let s sd sd1) ctx, tr)
    LetECtx s sd1 ctx   -> (SDZ (Let s sd1 sd) ctx, tr)
    IterCtx n tra m ctx -> (SDZ (Iterate n tra m sd) ctx, tr)
    VAssignCtx s assctx -> (AssignZ (s,sd) assctx, tr)
    LAssignSDCtx l prctx -> (LAssignGenZ l (ProgC sd prctx), tr)
  DoubleZ d   dctx   -> case dctx of
    RotDCtx  tctx       -> (TransZ (Rotate d) tctx, tr)
    ScDCtx   tctx       -> (TransZ (Scale d) tctx, tr)
    TransDLCtx d1 tctx  -> (TransZ (Translate (V2 d d1)) tctx, tr)
    TransDRCtx d1 tctx  -> (TransZ (Translate (V2 d1 d)) tctx, tr)
  IntZ    n   nctx   -> case nctx of
    IterSIntCtx tr1 l sd sdctx        -> (SDZ (Iterate n tr1 l sd) sdctx, tr)
    IterLIntCtx m tr1 sd [] after sdctx -> (TransZ tr1 (IterTransCtx m (n:after) sd sdctx), tr)
    IterLIntCtx m tr1 sd (x:xs) after sdctx -> (IntZ x (IterLIntCtx n tr1 sd xs (n:after) sdctx), tr)
  TransZ  t   tctx   -> case tctx of
    TCtx sd sdctx              -> (SDZ (T t sd) sdctx, tr)
    IterTransCtx n l sd sdctx  -> (SDZ (Iterate n t l sd) sdctx, tr)
  VarZ    v   vctx   -> case vctx of
    VarSDCtx sdctx          -> (SDZ (Var v) sdctx, tr)
    VarLetCtx sd1 sd2 sdctx -> (SDZ (Let v sd1 sd2) sdctx, tr)
    VarAssCtx sd assctx     -> (AssignZ (v, sd) assctx, tr)
  LAssignGenZ l lctx       -> z


topGenZ :: Prog -> (GenZipper, T2 Double)
topGenZ (PVars l)     = (LAssignGenZ l (ProgC (Pr SEmpty) TopGenCtx), mempty)
topGenZ (PSdia sd)    = (SDZ sd (LAssignSDCtx [] TopGenCtx), mempty)
topGenZ (ProgVS l sd) = (LAssignGenZ l (ProgC sd TopGenCtx), mempty)

makeGenZipper :: Prog -> (GenZipper, T2 Double)
makeGenZipper = topGenZ


downGenZ :: (GenZipper, T2 Double) -> (GenZipper, T2 Double)
downGenZ z@(gz, tr) = case gz of
  -- AssignZ (v, sd) assctx -> (VarZ v (VarAssCtx sd assctx), tr) -- go right
  AssignZ ass (AssignCtx l (x:xs) sd) -> (AssignZ x (AssignCtx (ass:l) xs sd), tr)
  AssignZ ass (AssignCtx l [] sd) -> z
  SDZ     sd  sdctx  -> case sd of
    Atop l r             -> (SDZ l (AtopLCtx sdctx r), tr)
    Let s sd1 sd2        -> (SDZ sd1 (LetVCtx s sdctx sd2), tr) --(VarZ s (VarLetCtx sd1 sd2 sdctx), tr)
    T (Scale d) sd       -> (SDZ sd (ScaleCtx d sdctx), tr)--(TransZ (Scale d) (TCtx sd sdctx), tr)
    T (Translate v) sd   -> (SDZ sd (TransCtx v sdctx), tr)--(TransZ (Translate v) (TCtx sd sdctx), tr)
    T (Rotate a) sd      -> (SDZ sd (RotateCtx a sdctx), tr)--(TransZ (Rotate a) (TCtx sd sdctx), tr)
    Iterate n tra l sd   -> (SDZ sd (IterCtx n tra l sdctx), tr)--(IntZ n (IterSIntCtx tra l sd sdctx), tr)
    -- VAssignCtx s assctx  -> (AssignZ (s, sd) assctx, tr)
    loc                  -> (SDZ loc Top, tr)
  DoubleZ d   dctx   -> case dctx of
    RotDCtx tr1           -> case tr1 of
      TCtx sd sdctx             -> (SDZ (T (Rotate d) sd) sdctx, tr)
      IterTransCtx n l sd sdctx -> (SDZ (Iterate n (Rotate d) l sd) sdctx, tr)
    ScDCtx tr1            -> case tr1 of
      TCtx sd sdctx  -> (SDZ (T (Scale d) sd) sdctx, tr)
      IterTransCtx n l sd sdctx -> (SDZ (Iterate n (Scale d) l sd) sdctx, tr)
    TransDRCtx d1 tr1     -> case tr1 of
      TCtx sd sdctx  -> (SDZ (T (Translate (V2 d1 d)) sd) sdctx, tr)
      IterTransCtx n l sd sdctx -> (SDZ (Iterate n (Translate (V2 d1 d)) l sd) sdctx, tr)
    TransDLCtx d1 tr1     -> case tr1 of
      TCtx sd sdctx  -> (SDZ (T (Translate (V2 d d1)) sd) sdctx, tr)
      IterTransCtx n l sd sdctx -> (SDZ (Iterate n (Translate (V2 d d1)) l sd) sdctx, tr)
  IntZ    n   nctx   -> case nctx of
    IterSIntCtx tra l sd sdctx               -> (TransZ tra (IterTransCtx n l sd sdctx), tr)
    IterLIntCtx m tra sd before [] sdctx     -> (SDZ sd (IterCtx m tra (reverse (n:before)) sdctx), tr)
    IterLIntCtx m tra sd before (x:xs) sdctx -> (IntZ x (IterLIntCtx m tra sd (n:before) xs sdctx), tr)
  TransZ  t   tctx   -> case (tctx, t) of
    (TCtx sd sdctx, Rotate a)    -> (SDZ sd (RotateCtx a sdctx), tr)
    (TCtx sd sdctx, Scale d)     -> (SDZ sd (ScaleCtx d sdctx), tr)
    (TCtx sd sdctx, Translate v) -> (SDZ sd (TransCtx v sdctx), tr)
    (IterTransCtx n (x:xs) sd sdctx, tr1) -> (IntZ x (IterLIntCtx n tr1 sd [] xs sdctx), tr)
    (IterTransCtx n [] sd sdctx, tr1)     -> (SDZ sd (IterCtx n tr1 [] sdctx), tr)
  VarZ    v   vctx   -> case vctx of
    VarSDCtx sdctx      -> case sdctx of
      ScaleCtx d ctx            -> (SDZ (T (Scale d) (Var v)) ctx, tr <> scaling d)
      RotateCtx a ctx           -> (SDZ (T (Rotate a) (Var v)) ctx, tr <> rotation (a @@ deg))
      TransCtx v1 ctx       -> (SDZ (T (Translate v1) (Var v)) ctx, tr <> translation v1)
      AtopLCtx ctx sd1          -> (SDZ (Atop (Var v) sd1) ctx, tr)
      AtopRCtx sd1 ctx          -> (SDZ (Atop sd1 (Var v)) ctx, tr)
      LetVCtx s ctx sd1         -> (SDZ (Let s (Var v) sd1) ctx, tr)
      LetECtx s sd1 ctx         -> (SDZ (Let s sd1 (Var v)) ctx, tr)
      IterCtx n tra m ctx       -> (SDZ (Iterate n tra m (Var v)) ctx, tr)
    VarLetCtx sd1 sd2 sdctx  -> (SDZ sd1 (LetVCtx v sdctx sd2), tr)
    VarAssCtx sd assctx      -> (SDZ sd (VAssignCtx v assctx), tr)  -- go right
  LAssignGenZ (x:xs) (ProgC sd prctx)  -> (AssignZ x (AssignCtx [] xs sd), tr)


rightGenZ :: (GenZipper, T2 Double) -> (GenZipper, T2 Double)
rightGenZ (gz, tr) = case gz of
  SDZ sd (AtopLCtx ctx sd1)          -> (SDZ sd1 (AtopRCtx sd ctx), tr)
  SDZ sd (LetVCtx s ctx sd1)         -> (SDZ sd1 (LetECtx s sd ctx), tr)
  SDZ (Iterate n tra l sd) sdctx     -> (IntZ n (IterSIntCtx tra l sd sdctx), tr)
  SDZ (T (Scale d) sd) ctx           -> (TransZ (Scale d) (TCtx sd ctx), tr)
  SDZ (T (Translate v) sd) ctx       -> (TransZ (Translate v) (TCtx sd ctx), tr)
  SDZ (T (Rotate a) sd) ctx          -> (TransZ (Rotate a) (TCtx sd ctx), tr)
  LAssignGenZ l (ProgC sd prctx)     -> (SDZ sd (LAssignSDCtx l prctx), tr)
  loc                                -> (loc, tr)


leftGenZ :: (GenZipper, T2 Double) -> (GenZipper, T2 Double)
leftGenZ (gz, tr) = case gz of
  SDZ sd (AtopRCtx sd1 ctx)           -> (SDZ sd1 (AtopLCtx ctx sd), tr)
  SDZ sd (LetECtx s sd1 ctx)          -> (SDZ sd1 (LetVCtx s ctx sd), tr)
  SDZ sd (LetVCtx s ctx sd1)          -> (VarZ s (VarLetCtx sd sd1 ctx), tr)
  SDZ sd (LAssignSDCtx l prctx)       -> (LAssignGenZ l (ProgC sd prctx), tr)
  IntZ n (IterSIntCtx tra l sd sdctx) -> (SDZ (Iterate n tra l sd) sdctx, tr)
  TransZ (Scale d) (TCtx sd ctx)      -> (SDZ (T (Scale d) sd) ctx, tr)
  TransZ (Translate v) (TCtx sd ctx)  -> (SDZ (T (Translate v) sd) ctx, tr)
  TransZ (Rotate a) (TCtx sd ctx)     -> (SDZ (T (Rotate a) sd) ctx, tr)
  loc                                 -> (loc, tr)



upmostGenZ :: (GenZipper, T2 Double) -> (GenZipper, T2 Double)
upmostGenZ z@(LAssignGenZ l (ProgC sd ctx), tr) = (SDZ sd (LAssignSDCtx l ctx), tr)
upmostGenZ z@(SDZ sd (LAssignSDCtx [] TopGenCtx), tr) = z
upmostGenZ z@(SDZ sd (LAssignSDCtx l TopGenCtx), tr)  = z
upmostGenZ z = upmostGenZ $ upGenZ z

-- editGenZ

unZipGenZ :: (GenZipper, T2 Double)-> Prog
unZipGenZ (LAssignGenZ [] (ProgC sd prctx), tr) = PSdia sd
unZipGenZ (LAssignGenZ l (ProgC (Pr SEmpty) prctx), tr) = PVars l
unZipGenZ (LAssignGenZ l (ProgC sd prctx), tr) = ProgVS l sd
unZipGenZ (SDZ sd (LAssignSDCtx [] prctx), tr) = PSdia sd
unZipGenZ (SDZ sd (LAssignSDCtx l prctx), tr) = ProgVS l sd
unZipGenZ z                                   = unZipGenZ $ upmostGenZ z


editZipper :: Modifyable a => (a -> a) -> (GenZipper, T2 Double) -> (GenZipper, T2 Double)
editZipper = editGenZ


cursify :: (GenZipper, T2 Double) -> (GenZipper, T2 Double)
cursify z@(SDZ _ _, _)     = editGenZ Cursor z
cursify z@(TransZ _ _, _)  = editGenZ CurTr z
cursify z                  = z


class Modifyable a where
  editGenZ :: (a -> a) -> (GenZipper, T2 Double) -> (GenZipper, T2 Double)

instance Modifyable SimpleDiagram where
  editGenZ f t@(gz, tr) = case gz of
    SDZ sd sdctx -> (SDZ (f sd) sdctx, tr)
    _            -> t

instance Modifyable Assign where
  editGenZ f t@(gz, tr) = case gz of
    AssignZ ass ctx -> (AssignZ (f ass) ctx, tr)
    _               -> t

instance Modifyable Double where
  editGenZ f t@(gz, tr) = case gz of
    DoubleZ d ctx   -> (DoubleZ (f d) ctx, tr)
    _               -> t

instance Modifyable Int where
  editGenZ f t@(gz, tr) = case gz of
    IntZ n ctx      -> (IntZ (f n) ctx, tr)
    _               -> t

instance Modifyable TransformationEdit where
  editGenZ f t@(gz, tr) = case gz of
    TransZ trans ctx -> (TransZ (f trans) ctx, tr)
    _                -> t

instance Modifyable String where
  editGenZ f t@(gz, tr) = case gz of
    VarZ s  ctx      -> (VarZ (f s) ctx, tr)
    _                -> t

instance Showzip (GenZipper, T2 Double) where
  showzip (gz, tr) = show gz
