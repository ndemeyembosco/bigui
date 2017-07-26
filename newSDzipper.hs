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
    (Showzip (..), TransformationEdit (..), Primitive (..), SimpleDiagram (..), SDCtx (..), SDzipper
     , Assign, Prog(..), ProgZipper, LAssignZipper, LAssignCtx, ProgCtx(..)
     ,leftP, rightP, upP, editZPS, editZP, makeProgZipper, unZipProgZ, unZipL
     , makeLAssignZipper, upLz, downLz
    , upZ, leftZ, rightZ
    , upmostZ, editZ, unZipSD, unZipWith, makeZipper, downZ, findTransform
    , GenZipper (..), LAssignGenCtx (..), ProgGenCtx (..), TransCtx (..)
    , DoubleCtx (..), IntCtx (..), VarCtx (..), AssignCtx (..)
    , upGenZ, topGenZ, makeGenZipper, downGenZ, rightGenZ, leftGenZ
    , upmostGenZ, unZipGenZ, editZipper, Modifyable (..)) where

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
  Scale     :: Double       -> TransformationEdit
  Translate :: V2 Double    -> TransformationEdit
  Rotate    :: Double       -> TransformationEdit
  deriving (Show, Eq)




data SDCtx where
  -- Above     ::
  -- TopMost   :: Assign         -> AssignCtx     -> SDCtx
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
upZ c@(sd, Top , tr)            = c
upZ (sd, ScaleCtx d ctx, tr)   = (T (Scale d) sd, ctx, tr <> inv (scaling d))
upZ (sd, TransCtx v ctx, tr)   = (T (Translate v) sd, ctx, tr <> inv (translation v))
upZ (sd, RotateCtx a ctx, tr)  = (T (Rotate a) sd, ctx, tr <> inv (rotation (a @@ deg)))
upZ (sd, AtopLCtx ctx sd1, tr) = (Atop sd sd1, ctx, tr)
upZ (sd, AtopRCtx sd1 ctx, tr) = (Atop sd1 sd, ctx, tr)
upZ (sd, LetVCtx s ctx sd1, tr)  = (Let s sd sd1, ctx, tr)
upZ (sd, LetECtx s sd1 ctx, tr)  = (Let s sd1 sd, ctx, tr)
upZ (sd, IterCtx n tra m ctx, tr) = (Iterate n tra m sd, ctx, tr)


topZ :: SimpleDiagram -> SDzipper     -- a.k.a makeZipper
topZ sd = (sd, Top, mempty)

makeZipper :: SimpleDiagram -> SDzipper
makeZipper = topZ


rightZ :: SDzipper -> SDzipper
rightZ (sd, AtopLCtx ctx sd1, tr)     =  (sd1, AtopRCtx sd ctx, tr)
rightZ (sd, LetVCtx s ctx sd1, tr) =  (sd1, LetECtx s sd ctx, tr)
rightZ loc                            = loc


leftZ :: SDzipper -> SDzipper
leftZ (sd, AtopRCtx sd1 ctx, tr)     = (sd1, AtopLCtx ctx sd, tr)
leftZ (sd, LetECtx s sd1 ctx, tr) = (sd1, LetVCtx s ctx sd, tr)
leftZ loc                            = loc


downZ :: SDzipper -> SDzipper
downZ (Atop l r, ctx, tr)             = (l, AtopLCtx ctx r, tr)   -- by default go left first.
downZ (Let s sd1 sd2, ctx, tr)     = (sd1, LetVCtx s ctx sd2, tr)
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


-----------------------------------------------------------------------------------------------------------
type Assign = (String, SimpleDiagram)

data Prog where
  PVars  :: [Assign]      -> Prog
  PSdia  :: SimpleDiagram -> Prog
  ProgVS :: [Assign]      -> SimpleDiagram -> Prog
  deriving (Show)


type ProgZipper = (Either LAssignZipper SDzipper, ProgCtx)

instance Showzip ProgZipper where
  showzip (Left l, prctx) = "(" ++ show l ++ ", " ++ showzip prctx ++ ")"
  showzip (Right sdz, prctx) = "(" ++ showzip sdz ++ ", " ++ showzip prctx ++ ")"

type LAssignZipper = (Assign, LAssignCtx)
type LAssignCtx = ([Assign], [Assign]) -- (above, below)

data ProgCtx where
  TopP     :: ProgCtx
  ProgVCtx :: ProgCtx -> SDzipper -> ProgCtx
  ProgSCtx :: LAssignZipper   -> ProgCtx -> ProgCtx

instance Showzip ProgCtx where
  showzip TopP = "TopP"
  showzip (ProgVCtx prctx sdz) = "ProgVCtx " ++ showzip prctx ++ " " ++ showzip sdz
  showzip (ProgSCtx l prctx)   = "ProgSCtx " ++ show l ++ " " ++ showzip prctx


leftP :: ProgZipper -> ProgZipper
leftP (Right sd, ProgSCtx l pctx) = (Left l, ProgVCtx pctx sd)
leftP p                           = p

rightP :: ProgZipper -> ProgZipper
rightP (Left l, ProgVCtx pctx sd) = (Right sd, ProgSCtx l pctx)
rightP p                          = p

upP :: ProgZipper -> ProgZipper
upP (Right szp, ctx) = (Right (upZ szp), ctx)
upP (Left l, ctx)    = (Left (upLz l), ctx)

downP :: ProgZipper -> ProgZipper
downP (Right szp, ctx) = (Right (downZ szp), ctx)
downP (Left l, ctx)    = (Left (downLz l), ctx)



editZPS :: (SDzipper -> SDzipper) -> ProgZipper -> ProgZipper
editZPS f (Right sdz, ctx) = (Right (f sdz), ctx)
editZPS f (Left l, ProgVCtx pctx sdz) = (Right (f sdz), ProgSCtx l pctx)
editZPS f z                           = z

editZP :: (Either LAssignZipper SDzipper -> Either LAssignZipper SDzipper) -> ProgZipper -> ProgZipper
editZP f (z, pctx)     = (f z, pctx)


makeProgZipper :: Prog -> ProgZipper
makeProgZipper (PVars l)     = (Left (makeLAssignZipper l), TopP)
makeProgZipper (PSdia sd)    = (Right (makeZipper sd), TopP)
makeProgZipper (ProgVS l sd) = (Right (makeZipper sd), ProgSCtx (makeLAssignZipper l) TopP)

unZipProgZ :: ProgZipper -> Prog
unZipProgZ prz = case prz of
  (Right sdz, ProgSCtx l ctx) -> ProgVS (unZipL l) (unZipSD sdz)
  (Left l, ProgVCtx ctx sd)   -> ProgVS (unZipL l) (unZipSD sd)
  (Right sdz, TopP)           -> PSdia (unZipSD sdz)  -- probably wrong?
  (Left lz, TopP)             -> PVars (unZipL lz)    -- probably wrong?

unZipL :: LAssignZipper -> [Assign]
unZipL (as, (labove, lbelow)) = labove ++ [as] ++ lbelow

makeLAssignZipper :: [Assign] -> LAssignZipper
makeLAssignZipper l = (head l, ([], tail l))

upLz :: LAssignZipper -> LAssignZipper
upLz z@(as, (labove, lbelow)) = case labove of
  []       -> z
  l@(x:xs) -> (last l, (drop ((length l) - 1) l, as : lbelow))

downLz :: LAssignZipper -> LAssignZipper
downLz z@(as, (labove, lbelow)) = case lbelow of
  []        -> z
  l@(x:xs)  -> (x, (labove ++ [as], xs))



----------------------------------------------------------------------------------


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
  AssignCtx :: [Assign] -> [Assign] -> AssignCtx
  deriving (Show)

upGenZ :: (GenZipper, T2 Double) -> (GenZipper, T2 Double)
upGenZ z@(gz, tr) = case gz of
  TopGenZ            -> z
  AssignZ ass assctx -> case assctx of
    (AssignCtx [] r)      -> z
    (AssignCtx (x:xs) r)  -> (AssignZ x (AssignCtx xs (ass:r)), tr)
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
  AssignZ ass (AssignCtx l (x:xs)) -> (AssignZ x (AssignCtx (ass:l) xs), tr)
  AssignZ ass (AssignCtx l []) -> z
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
  LAssignGenZ (x:xs) lassctx  -> (AssignZ x (AssignCtx [] xs), tr)
  -- LAssignGenZ []




rightGenZ :: (GenZipper, T2 Double) -> (GenZipper, T2 Double)
rightGenZ (gz, tr) = case gz of
  SDZ sd (AtopLCtx ctx sd1)          -> (SDZ sd1 (AtopRCtx sd ctx), tr)
  SDZ sd (LetVCtx s ctx sd1)         -> (SDZ sd1 (LetECtx s sd ctx), tr)
  SDZ sd (IterCtx n tra l sdctx)     -> (IntZ n (IterSIntCtx tra l sd sdctx), tr)
  SDZ sd (ScaleCtx d ctx)            -> (TransZ (Scale d) (TCtx sd ctx), tr)
  SDZ sd (TransCtx v ctx)            -> (TransZ (Translate v) (TCtx sd ctx), tr)
  SDZ sd (RotateCtx a ctx)           -> (TransZ (Rotate a) (TCtx sd ctx), tr)
  LAssignGenZ l (ProgC sd prctx)     -> (SDZ sd (LAssignSDCtx l prctx), tr)
  loc                                -> (loc, tr)


  -- loc                        ->

leftGenZ :: (GenZipper, T2 Double) -> (GenZipper, T2 Double)
leftGenZ (gz, tr) = case gz of
  SDZ sd (AtopRCtx sd1 ctx)           -> (SDZ sd1 (AtopLCtx ctx sd), tr)
  SDZ sd (LetECtx s sd1 ctx)          -> (SDZ sd1 (LetVCtx s ctx sd), tr)
  SDZ sd (LetVCtx s ctx sd1)          -> (VarZ s (VarLetCtx sd sd1 ctx), tr)
  SDZ sd (LAssignSDCtx l prctx)       -> (LAssignGenZ l (ProgC sd prctx), tr)
  IntZ n (IterSIntCtx tra l sd sdctx) -> (SDZ sd (IterCtx n tra l sdctx), tr)
  TransZ (Scale d) (TCtx sd ctx)      -> (SDZ sd (ScaleCtx d ctx), tr)
  TransZ (Translate v) (TCtx sd ctx)  -> (SDZ sd (TransCtx v ctx), tr)
  TransZ (Rotate a) (TCtx sd ctx)     -> (SDZ sd (RotateCtx a ctx), tr)
  loc                                 -> (loc, tr)



upmostGenZ :: (GenZipper, T2 Double) -> (GenZipper, T2 Double)
upmostGenZ z@(LAssignGenZ l (ProgC sd ctx), tr) = (SDZ sd (LAssignSDCtx l ctx), tr)
upmostGenZ z@(SDZ sd (LAssignSDCtx [] TopGenCtx), tr) = z
-- upmostGenZ z@(SDZ sd (LAssignSDCtx l TopGenCtx), tr)  = z
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
