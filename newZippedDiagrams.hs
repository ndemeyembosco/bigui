{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}




import Control.Monad
import qualified Control.Applicative as A



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
import qualified Data.Map as M

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
import           NewSDzipper
import           MyParser
-- import           KeyCode

{- ------------- set all other parts of diagram to false execpt those under the cursor, which
will also be rendered thicker than others. ---------------------- -}


interpSimpleDiagram :: Env -> SimpleDiagram -> QDiagram SVG V2 Double Any
interpSimpleDiagram e (Pr pr) = case pr of
  SEmpty      -> mempty
  Circle      -> circle 1
  Triangle    -> triangle 1
  Square      -> square 1
  Polygon sds -> regPoly sds 1
interpSimpleDiagram e (Atop sdl sdr) = interpSimpleDiagram e sdl `atop` interpSimpleDiagram e sdr
interpSimpleDiagram e (T tr sd) = interpSimpleDiagram  e sd # transform (findTransform tr)
interpSimpleDiagram e (Iterate n tra m sd) = case m of
  Nothing -> mconcat $ iterateN n (transform $ findTransform tra) (interpSimpleDiagram  e sd)
  Just t  -> let l = iterateN n (transform $ findTransform tra) (interpSimpleDiagram  e sd) in (mconcat $  deleteDiagFList (t - 1) l)
interpSimpleDiagram e (Cursor sd)      = (interpSimpleDiagram  e sd  # lw veryThick)
interpSimpleDiagram e (Assign s sd1 sd2)    = interpSimpleDiagram (M.insert s sd1 e) sd2
interpSimpleDiagram e (Var s)          = case M.lookup s e of
  Nothing -> mempty
  Just sd -> interpSimpleDiagram e sd

deleteDiagFList :: Int -> [QDiagram SVG V2 Double Any] -> [QDiagram SVG V2 Double Any]
deleteDiagFList n l = let l1 = splitAt n l in fst l1 ++ (tail $ snd $ splitAt n l)

atopify :: [QDiagram SVG V2 Double Any] -> QDiagram SVG V2 Double Any
atopify = foldr (\sd1 sd2 -> atop sd1 sd2) mempty

parseSVG :: String -> String
parseSVG [] = []
parseSVG s@(x:xs)
             |take 4 s == "<svg" = s
             |otherwise =  parseSVG xs
--
--
diagTuple' :: QDiagram SVG V2 Double Any -> (T2 Double, Element)
diagTuple' myDiag' = renderDiaT SVG (SVGOptions (mkWidth 500) Nothing "" [] True) myDiag'
--
renderedString' :: QDiagram SVG V2 Double Any -> (T2 Double, String)
renderedString' myDiag = case diagTuple' myDiag of
 (t, rendered) -> (t, DT.unpack $ renderText $ rendered)
--
-- -------------------------------------------------------------------------------------------------
parseCoord :: Parser (V2 Double)
parseCoord = r2 <$> ((,) <$> (mysymbol "(" *> mydouble <* mysymbol ",") <*> (mydouble <* mysymbol ")"))
--
parseCursor :: Parser SimpleDiagram
parseCursor = myreserved "==>" *> parseSimpleDiagram

parseTransformEdit :: Parser TransformationEdit
parseTransformEdit = try (Translate <$> (myreserved "translate" *> parseCoord))
                     <|> (Scale <$> (myreserved "scale" *> mydouble))
                     <|> (Rotate <$> (myreserved "rotate" *> mydouble))

parseSplit :: Parser Int
parseSplit = mybrackets (mysymbol "/" *> myinteger)


parseAssign :: Parser SimpleDiagram
parseAssign = Assign <$> (myreserved "let" *> ident) <*> (myreserved "=" *> parseSimpleDiagram) <*> (myreserved "in" *> parseSimpleDiagram)

parseIterate :: Parser SimpleDiagram
parseIterate = Iterate <$> (myreserved "iterate" *> myinteger) <*> parseTransformEdit <*> optionMaybe parseSplit <*> parseSimpleDiagram

parseSimpleDiagram :: Parser SimpleDiagram
parseSimpleDiagram = (Pr Circle) <$ myreserved "circle"
    <|> (Pr Triangle)  <$ myreserved "triangle"
    <|> (Pr Square)    <$ myreserved "square"
    <|> Pr <$> (Polygon  <$> (myreserved "polygon" *> myinteger))
    <|> T <$> parseTransformEdit <*> parseSimpleDiagram
    <|> Atop <$> (myreserved "atop" *> parseSimpleDiagram) <*> (parseSimpleDiagram)
    <|> parseAssign
    <|> parseIterate
    <|> myparens parseSimpleDiagram
    <|> parseCursor
    <|> try (Var <$> ident)

parseProg :: Parser Prog
parseProg = Prog <$> optionMaybe parseVarAssigns <*> parseSimpleDiagram

parseVarAs :: Parser VarAssign
parseVarAs = VarAs <$> ident <*> parseSimpleDiagram

parseVarAssigns :: Parser VarAssignments
parseVarAssigns = mysemiSep1 parseVarAs


interpProg :: Prog -> QDiagram SVG V2 Double Any
interpProg (Prog Nothing sd)   = interpSimpleDiagram M.empty sd
interpProg (Prog (Just as) sd) = interpSimpleDiagram (interpAssigns as) sd

interpAssigns :: VarAssignments -> M.Map String SimpleDiagram
interpAssigns = M.fromList.fmap (\(VarAs s sd) -> (s, sd))


evalExpr' :: String -> Maybe (QDiagram SVG V2 Double Any)
evalExpr' s = case myparse parseSimpleDiagram s of
  Right sd -> Just (interpSimpleDiagram M.empty sd)
  Left _   -> Nothing

-- evalExpr' :: String -> Maybe (QDiagram SVG V2 Double Any)
-- evalExpr' s = case myparse parseProg s of
--   Right pr -> Just (interpProg pr)
--   Left _   -> Nothing
--
isCompileReady :: String -> Bool
isCompileReady s = case evalExpr' s of
  Nothing -> False
  Just _  -> True
--
-- ---------------------------------------------------------------------------------------
--
data SDdata where
  FrmCode   :: String   -> SDdata
  DragCoord :: V2 Double -> SDdata
  Click     :: P2 Double -> SDdata
  Nav       :: DIRECTION -> SDdata
  Split     :: Int       -> SDdata
  deriving (Show)
--
data DIRECTION = LEFT | RIGHT | UP | DOWN
  deriving (Show, Eq)
--
--
--
--
main :: IO ()
main = do
  T.startGUI T.defaultConfig setup

setup :: T.Window -> T.UI ()
setup window = void $ mdo

  -- GUI components set up and styling
  diagWindow         <- UI.div T.#. "diagram" # T.set T.style [("width", "520px"), ("height", "520px"), ("border", "1px solid #000")]
  codeArea           <- UI.textarea T.# T.set (T.attr "rows") "20" T.# T.set (T.attr "cols") "100"
  debuggArea         <- UI.div T.#. "debugg" # T.set T.style [("width", "500px"), ("height", "100px"), ("border", "1px solid #000")]
  debuggArea2        <- UI.div T.#. "debugg2" # T.set T.style [("width", "500px"), ("height", "100px"), ("border", "1px solid #000")]
  upButton           <- UI.button T.# T.set T.text "up" # T.set T.style [("width", "50px"), ("height", "20px")]
  downButton         <- UI.button T.# T.set T.text "down" # T.set T.style [("width", "50px"), ("height", "20px")]
  leftButton         <- UI.button T.# T.set T.text "left" # T.set T.style [("width", "50px"), ("height", "20px")]
  rightButton        <- UI.button T.# T.set T.text "right" # T.set T.style [("width", "50px"), ("height", "20px")]
  textfield          <- UI.input T.# T.set (T.attr "type") "text" T.# T.set (T.attr "value") "0"
  splitTextfield     <- UI.row [UI.string "split: ", T.element textfield]
  splitButton        <- UI.button T.# T.set T.text "split" # T.set T.style [("width", "50px"), ("height", "20px")]
  T.getBody window T.#+ [UI.grid [[UI.column [T.element diagWindow, T.element debuggArea]
                        , UI.column [T.element codeArea, UI.row [T.element upButton, T.element downButton]
                        , UI.row [T.element leftButton, T.element rightButton], UI.row [T.element splitButton
                        , T.element splitTextfield], T.element debuggArea2]]]]
  bodyWindow         <- T.getBody window

  let

  -- Track all user input events
      codeAreaE           = UI.valueChange codeArea
      mouseUpE            = UI.mouseup diagWindow
      mousedownE          = UI.mousedown diagWindow
      mousemoveE          = UI.mousemove diagWindow
      upClickE            = UP    <$ UI.click upButton
      downClickE          = DOWN  <$ UI.click downButton
      leftClickE          = LEFT  <$ UI.click leftButton
      rightClickE         = RIGHT <$ UI.click rightButton
      splitClickE         = UI.click splitButton
      (splitInputE :: T.Event [(Int, String)])        = readInt <$> UI.valueChange textfield
      spaceE              = (==32) <$> UI.keydown codeArea

      -- splitInputE'        = head <$> splitInputE

      -- transform point into diagram coordinates and generate cursors events from it
      mousedownTrE        = transform <$> (inv <$> transformB) T.<@> (makePoint <$> mousedownE)
      cursorE             = sample    <$> q2DiagramB           T.<@> mousedownTrE

      -- to avoid lagging zip the point and cursor obtained from it into one event
      (pointPathTupleE :: T.Event (P2 Double, Any))                   = zipE mousedownTrE cursorE
      mouseOutTupleE                                                  = T.filterE (\(p, cursor) -> getAny cursor == False) pointPathTupleE
      mouseInTupleE                                                   = T.filterE (\(p, cursor) -> getAny cursor == True) pointPathTupleE


      -- helper events for what it means to be dragging
      ismousedownE                                                    = True <$ mouseInTupleE
      (ismouseUpE   :: T.Event Bool)                                  = False <$ mouseUpE
      (ismouseDragE :: T.Event Bool)                                  = T.unionWith (&&) ismousedownE ismouseUpE

      -- again to avoid lagging, put point and drag flag associated with it into one event
      (pointMoveBoolE :: T.Event (P2 Double, Bool))                   = zipE (makePoint <$> mousemoveE) ismouseDragE

  ismouseDragB                                   <- T.stepper False ismouseDragE
  cursorB                                        <- T.stepper mempty cursorE
  mousedownTrB                                   <- T.stepper (makePoint (0, 0)) mousedownTrE
  codeAreaChangedB                               <- T.stepper "" codeAreaE
  splitInputB                                    <- T.stepper [] splitInputE

  let
   -- untransformed and transformed clicking, dragging and scaling events
      mouseOutE        = fst <$> mouseOutTupleE
      draggingE        = T.whenE ismouseDragB mousemoveE
      mvPointsE        = transform <$> (inv <$> transformB) T.<@> (makePoint <$> draggingE)
      -- codeAreaE'       = T.whenE
    -- merge mousedown event with drag event to avoid jumping when dragging resumes
      unionEventMvDown = T.unionWith const (fst <$> mouseInTupleE) mvPointsE

  bPrevMousePt                                     <- T.stepper origin unionEventMvDown

  let

  -- simulate dragging
      (translations       :: T.Event (V2 Double))                     = fmap (\prev cur -> cur .-. prev) bPrevMousePt T.<@> mvPointsE

  {- merge all possible edits to the diagram into one data type
  , run the edits and generate behavior of obtained simpleDiagram -}

      (formedSDdataE  :: T.Event SDdata)                              = mergeEvents codeAreaE translations mouseOutE upClickE downClickE leftClickE rightClickE splitInputE
      (simpleDiagramE :: T.Event (SDzipper -> SDzipper))              = T.filterJust (runSDdata <$> formedSDdataE)

  (simpleDiagramB'  :: T.Behavior SDzipper)     <- T.accumB (Pr SEmpty, Top, mempty) simpleDiagramE
  testBehavior                                  <- T.stepper (V2 0.0 0.0) translations


  let
  -- render diagram
      simpleDiagramB = unZipSD <$> (editZ Cursor <$> simpleDiagramB')
      dTupleB        = makeDToDraw <$> simpleDiagramB
      diagramStrB    = fmap (\(x, y, z) -> x) dTupleB
      transformB     = fmap (\(x, y, z) -> y) dTupleB
      q2DiagramB     = fmap (\(x, y, z) -> z) dTupleB
      -- debuggStrB     = (show <$> simpleDiagramB')
      codeAreaStrB   = (pprintTree <$> simpleDiagramB)


  -- Sink diagram behavior into appropriate gui elements
  T.element debuggArea2 # T.sink UI.text (showzip <$> simpleDiagramB')
  -- T.element debuggArea # T.sink UI.text debuggStrB
  T.element diagWindow # T.sink UI.html diagramStrB
  T.element codeArea   # T.sink UI.value codeAreaStrB

  return ()




------------------------------- helpers  ----------------------------------------


-- merge two events that happen at the same time to avoid lagging.
zipE :: T.Event a -> T.Event b -> T.Event (a, b)
zipE e1 e2 = fmap (\(Just s, Just t) -> (s, t)) (T.unionWith (\(Just p, Nothing) (Nothing, Just pth) -> (Just p, Just pth)) (fmap (\p -> (Just p, Nothing)) e1)
                                                       (fmap (\pth -> (Nothing, Just pth)) e2))

-- render syntax tree
makeDToDraw :: SimpleDiagram -> (String, (T2 Double), (QDiagram SVG V2 Double Any))
makeDToDraw sd = let code = interpSimpleDiagram M.empty sd # withEnvelope (square 10 :: Diagram B) in let (tr, svgStr) = renderedString' code in (parseSVG $ svgStr, tr, code)



-- handle different kinds of edits to the diagram
runSDdata :: SDdata -> Maybe (SDzipper -> SDzipper)
runSDdata (FrmCode str)   = case myparse parseSimpleDiagram str of
  Right sd -> Just $ const (makeZipper sd)
  Left  _  -> Nothing
runSDdata (DragCoord cd)  = Just $ \zp@(sd, ctx, tr) -> refactorTree tr cd zp
runSDdata (Click pt)      = Just $ \sd@(sd1, ctx, tr) -> editZ (createNewDiagram (transform (inv tr) pt) sd1) sd
runSDdata (Nav k)  = Just $ \zp -> navigateTree k zp
runSDdata (Split n)  = Just $ \zp -> editZ (splitTree n) zp

newCircleCreation :: P2 Double -> SimpleDiagram -> SimpleDiagram
newCircleCreation pt (Pr SEmpty) = createNewCircle pt
newCircleCreation pt sd          = Atop (createNewCircle pt) sd

createNewDiagram :: P2 Double -> SimpleDiagram -> SimpleDiagram -> SimpleDiagram
createNewDiagram pt sd1 sd = case sd1 of
  Pr SEmpty -> createNewCircle pt
  _         -> Atop (T (Translate (pt .-. origin)) sd1) sd



-- creating and new circle, to be used on click outside of diagram.
createNewCircle :: P2 Double -> SimpleDiagram
createNewCircle p = T (Translate (p .-. origin)) (Pr Circle)

-- -> T.Event Int

-- merge all different kinds of edits to the diagram into one data type.
mergeEvents :: T.Event String -> T.Event (V2 Double) -> T.Event (P2 Double)
               -> T.Event DIRECTION -> T.Event DIRECTION -> T.Event DIRECTION -> T.Event DIRECTION -> T.Event [(Int, String)] -> T.Event SDdata
mergeEvents e1 e2 e3 e4 e5 e6 e7 e8 = head <$> T.unions [FrmCode <$> e1, DragCoord <$> e2, Click <$> e3, Nav <$> e4, Nav <$> e5, Nav <$> e6, Nav <$> e7, fmap (\l -> Split $ fst $ head l) e8]

-- turn returned coordinate into point
makePoint :: (Int, Int) -> P2 Double
makePoint (x, y) = p2 (fromIntegral x, fromIntegral y)


--- probably wrong.

refactorTree :: T2 Double -> V2 Double -> SDzipper -> SDzipper
refactorTree tr v sz = editZ (refactorTree' (transform (inv tr) v)) sz   -- use inv transformations

refactorTree' :: V2 Double -> SimpleDiagram -> SimpleDiagram
refactorTree'  p (T (Translate v) sd)          = T (Translate (p ^+^ v)) sd
-- refactorTree'  (V2 d1 d2) sc@(Scale d sd)  = Translate (V2 (d1 / d) (d2 / d)) sc
refactorTree'  v  sd                       = T (Translate v) sd




splitTree :: Int -> SimpleDiagram -> SimpleDiagram
splitTree n zp@(Iterate m tra may sd) = case may of
  Nothing -> case tra of
    Scale d     -> (Atop (T (Scale (d^n)) sd) (Iterate n tra (Just n) sd))
    Translate (V2 v1 v2) -> let m = fromIntegral n in (Atop (T (Translate (V2 (v1*m) (v2*m))) sd) (Iterate n tra (Just n) sd))
    Rotate a    -> let m = fromIntegral n in (Atop (T (Rotate (a*m)) sd) (Iterate n tra (Just n) sd))
  Just t  -> undefined
splitTree n sd                        = sd

{- handle control-key presses -}
navigateTree :: DIRECTION -> SDzipper -> SDzipper
navigateTree k z
                  |k == DOWN  = downZ  z
                  |k == UP    = upZ    z
                  |k == LEFT  = leftZ  z
                  |k == RIGHT = rightZ z
                  |otherwise  = z


{- pretty print the updated syntax tree
   in order to sink it in the textarea.
 -}

pprintVec :: V2 Double -> String
pprintVec (V2 d1 d2) = "(" ++ printf "%.3f" d1 ++ "," ++ printf "%.3f" d2 ++ ")"

pprintPrim :: Primitive -> String
pprintPrim (SEmpty) = ""
pprintPrim (Circle) = "circle"
pprintPrim (Square) = "square"
pprintPrim (Triangle) = "triangle"
pprintPrim (Polygon n)   = "polygon " ++ show n

pprintTransfromEdit :: TransformationEdit -> String
pprintTransfromEdit (Scale d) = "scale " ++ show d
pprintTransfromEdit (Translate v) = "translate " ++ pprintVec v
pprintTransfromEdit (Rotate a)    = "rotate " ++ show a

pprintTree ::  SimpleDiagram -> String
pprintTree = pprintTree' 0

replicateSp :: Int -> String
replicateSp n = foldr (++) [] (replicate n "\t")


--- keep track of boolean cursor, put "->" on meeting it.

pprintTree' ::  Int -> SimpleDiagram -> String
pprintTree' n (Var s)            = replicateSp n ++ s
pprintTree' n (Assign s sd1 sd2) = replicateSp n ++ "let " ++ s ++ " = \n" ++ pprintTree' (n + 1) sd1 ++ " in \n" ++ pprintTree' (n + 1) sd2
pprintTree' n (Pr d)             = replicateSp n ++ (pprintPrim d)
pprintTree' n (T tra sd)         = replicateSp n ++ (pprintTransfromEdit tra) ++ "\n" ++  pprintTree' (n + 1) sd
pprintTree' n (Atop sdl sdr)     = replicateSp n ++"atop " ++ "\n"  ++ pprintTree' (n + 1) sdl  ++ "\n"  ++  pprintTree' (n + 1) sdr
pprintTree' n (Iterate m tra may sd) = case may of
  Nothing -> replicateSp n ++ "iterate " ++ show m ++ " " ++  pprintTransfromEdit tra  ++ "\n"  ++  pprintTree' (n + 1) sd
  Just t  -> replicateSp n ++ "iterate " ++ show m ++ " " ++  pprintTransfromEdit tra  ++ " [/" ++ show t ++ "]" ++ "\n"  ++  pprintTree' (n + 1) sd
pprintTree' n (Cursor sd)        = case sd of
  (Pr SEmpty)   -> pprintTree' n sd
  _             -> "==> " ++ pprintTree' n sd

pprintProg :: Prog -> String
pprintProg = pprintProg' 0


pprintProg' :: Int -> Prog -> String
pprintProg' n (Prog Nothing sd)   = pprintTree' n sd
pprintProg' n (Prog (Just as) sd) = pprintAssigns as ++ "\n" ++ pprintTree' n sd

pprintAssigns :: VarAssignments -> String
pprintAssigns = show.fmap (\l -> show l ++ ";\n")

readInt :: String -> [(Int, String)]
readInt s = (reads s :: [(Int, String)])
