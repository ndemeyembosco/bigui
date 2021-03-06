{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}




import Control.Monad
import qualified Control.Applicative as A



import Graphics.Svg.Core
import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.Core as T

import Data.Functor
import Data.List
import Data.Maybe
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
import           Text.Read

assignParse :: Parser Assign
assignParse = (,) <$> ident <*> (myreserved "=" *> parseSimpleDiagram)

parseProg :: Parser Prog
parseProg =  try (ProgVS <$> (myreserved "#" *> (mysemiSep1 assignParse) <* myreserved "#") <*> parseSimpleDiagram)
         <|> PSdia  <$> parseSimpleDiagram
         <|> try (PVars  <$> (myreserved "#" *> (mysemiSep1 assignParse)  <* myreserved "#") <* eof)


assingInterp :: Env -> Assign -> (String, QDiagram SVG V2 Double Any)
assingInterp e (s, d) = (s, interpSimpleDiagram e d)

interpVarSection :: Env -> [Assign] -> Env
interpVarSection e []     = e
interpVarSection e (x:xs) = let (s, d) = assingInterp e x in interpVarSection (M.insert s d e) xs

interpProg :: Env -> Prog -> QDiagram SVG V2 Double Any
interpProg e (PVars l)     = interpSimpleDiagram (interpVarSection e l) (Pr SEmpty)
interpProg e (PSdia sd)    = interpSimpleDiagram e sd
interpProg e (ProgVS l sd) = let env = interpVarSection e l in interpSimpleDiagram env sd


------------------------------------- Prog Zipper --------------------------------------------------
type Env   = M.Map String (QDiagram SVG V2 Double Any)

interpSimpleDiagram :: Env -> SimpleDiagram -> QDiagram SVG V2 Double Any
interpSimpleDiagram e (Pr pr) = case pr of
  SEmpty      -> mempty
  Circle      -> circle 1
  Triangle    -> triangle 1
  Square      -> square 1
  Polygon sds -> regPoly sds 1
interpSimpleDiagram e (Atop sdl sdr) = interpSimpleDiagram e sdl `atop` interpSimpleDiagram e sdr
interpSimpleDiagram e (T tr sd) = interpSimpleDiagram  e sd # transform (findTransform tr)
interpSimpleDiagram e (Iterate n tra m sd) = let l = iterateN n (transform $ findTransform tra) (interpSimpleDiagram  e sd) in (mconcat $ deleteLDiagFList m l)
interpSimpleDiagram e (Cursor sd)      = (interpSimpleDiagram  e sd  # lw veryThick)
interpSimpleDiagram e (Let s sd1 sd2)    = interpSimpleDiagram (M.insert s (interpSimpleDiagram e sd1) e) sd2
interpSimpleDiagram e (Var s)          = case M.lookup s e of
  Nothing -> mempty
  Just sd -> sd

deleteDiagFList :: Int -> [QDiagram SVG V2 Double Any] -> [QDiagram SVG V2 Double Any]
deleteDiagFList n l = let l1 = splitAt n l in fst l1 ++ (tail $ snd $ splitAt n l)

deleteLDiagFList :: [Int] -> [QDiagram SVG V2 Double Any] -> [QDiagram SVG V2 Double Any]
deleteLDiagFList l ql = foldr (\n list -> deleteDiagFList (n - 1) list) ql l

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
                     <|> parseCrTr

parseCrTr :: Parser TransformationEdit
parseCrTr = myreserved "+" *> parseTransformEdit

parseSplit :: Parser [Int]
parseSplit = mybrackets (mysymbol "/" *> mycommaSep1 myinteger)


parseAssign :: Parser SimpleDiagram
parseAssign = Let <$> (myreserved "let" *> ident) <*> (myreserved "=" *> parseSimpleDiagram) <*> (myreserved "in" *> parseSimpleDiagram)

parseIterate :: Parser SimpleDiagram
parseIterate = Iterate <$> (myreserved "iterate" *> myinteger) <*> parseTransformEdit <*> (handleSplit <$> optionMaybe parseSplit) <*> parseSimpleDiagram
        where
          handleSplit Nothing  = []
          handleSplit (Just l) = l

parseSimpleDiagram :: Parser SimpleDiagram
parseSimpleDiagram = (Pr Circle) <$ myreserved "circle"
    <|> (Pr Triangle)  <$ myreserved "triangle"
    <|> (Pr Square)    <$ myreserved "square"
    <|> Pr <$> (Polygon  <$> (myreserved "polygon" *> myinteger))
    <|> T <$> parseTransformEdit <*> parseSimpleDiagram
    <|> try (Atop <$> (myreserved "atop" *> parseSimpleDiagram) <*> (parseSimpleDiagram))
    <|> parseAssign
    <|> parseIterate
    <|> myparens parseSimpleDiagram
    <|> parseCursor
    <|> try (Var <$> ident)


evalExpr' :: String -> Maybe (QDiagram SVG V2 Double Any)
evalExpr' s = case myparse parseProg s of
  Right pr -> Just (interpProg M.empty pr)
  Left _   -> Nothing
--
isCompileReady :: String -> Bool
isCompileReady s = case evalExpr' s of
  Nothing      -> False
  Just _       -> True
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


data DIRECTION = LEFT | RIGHT | UP | DOWN
  deriving (Show, Eq)


main :: IO ()
main = do
  T.startGUI T.defaultConfig setup

setup :: T.Window -> T.UI ()
setup window = void $ mdo
  UI.addStyleSheet window ""

  -- GUI components set up and styling
  diagWindow         <- UI.div T.#. "diagram" T.# T.set T.style [("width", "48%")
                                                           , ("height", "450px")
                                                           , ("border", "1px solid #000")
                                                           , ("float", "left")]

  codeArea           <- UI.textarea T.# T.set T.style [  ("height", "98%")
                                                       , ("width", "98%")
                                                       , ("padding", "1%")
                                                       , ("border", "1px solid #000")
                                                      ]

  -- text fields

  scaleTxtfld        <- UI.input T.#. "scaleTxtfld" T.# attributeList [("type", "text")
                                                                       , ("placeholer", "0")
                                                                       ]
  rotateTxtfld      <- UI.input T.#. "rotateTxtfld" T.# attributeList [("type", "text")
                                                                       , ("placeholer", "0")
                                                                       ]
  iterateTxtfld     <- UI.input T.#. "iterateTxtfld" T.# attributeList [("type", "text")
                                                                       , ("placeholer", "0")
                                                                       ]

  transformTxtfld      <- UI.input T.#. "transformTxtfld" T.# attributeList [("type", "text")
                                                                       , ("placeholer", "0")
                                                                       ]

  iterCountTxtfld      <- UI.input T.#. "iterCountTxtfld" T.# attributeList [("type", "text")
                                                                       , ("placeholer", "0")
                                                                       ]
  -- buttons
  upButton          <- UI.button T.#. "upButton"    T.# T.set T.html "UP" T.# T.set T.style [("width", "48%")
                                                                                             , ("float", "left")
                                                                                             , ("margin-bottom", "3px")]
  downButton        <- UI.button T.#. "downButton"  T.# T.set T.html "DOWN" T.# T.set T.style [("width", "48%")
                                                                                             , ("float", "left")]
  leftButton        <- UI.button T.#. "leftButton"  T.# T.set T.html "LEFT" T.# T.set T.style [("width", "48%")
                                                                                             , ("float", "right")
                                                                                             , ("margin-bottom", "3px")]
  rightButton       <- UI.button T.#. "rightButton" T.# T.set T.html "RIGHT" T.# T.set T.style [("width", "48%")
                                                                                             , ("float", "right")]
  -- forms
  navForm           <- UI.div T.#. "navForm" T.#+ [UI.thehtml T.# T.set T.html "<h3>Navigate AST</h3><br>"
                                                               T.# T.set T.style [("text-align", "center")]
                                                    , UI.div T.#. "row-1" T.# T.set T.style [("width", "100%")] T.#+ [T.element upButton
                                                           , T.element leftButton]
                                                    , UI.div T.#. "row-1" T.# T.set T.style [("width", "100%")] T.#+ [T.element downButton
                                                           , T.element rightButton]
                                                   ]

  debuggArea2          <- UI.div T.#. "debugger" T.# T.set T.style [("width", "100%")
                                                                , ("height", "3em")
                                                                , ("border", "3px")
                                                                , ("border-style", "dotted")
                                                                , ("border-color", "coral")]

  transfForm        <- UI.div T.#. "transfForm" T.#+ [UI.thehtml T.# T.set T.html "<h3>Transform</h3><br>"
                                                                  T.# T.set T.style [("text-align", "center")]
                                                     , UI.row [UI.thehtml T.# T.set T.html "Scale:"
                                                               , T.element scaleTxtfld
                                                               , UI.thehtml T.# T.set T.html "Rotate:"
                                                               , T.element rotateTxtfld]
                                                     , UI.thehtml T.# T.set T.html "<h4>Iterate:</h4><br>"
                                                                  T.# T.set T.style [("text-align", "center")]
                                                     , UI.row [UI.thehtml T.# T.set T.html "transformation:"
                                                               , T.element transformTxtfld
                                                               , UI.thehtml T.# T.set T.html "Iteration count:"
                                                               , T.element iterCountTxtfld]
                                                      , UI.row [T.element debuggArea2]
                                                     ]


  -- window/body
  T.getBody window T.#+ [UI.div T.#. "outer" T.# T.set T.style [("width", "100%")] T.#+ [T.element diagWindow,
                                  -- UI.div T.#. "empty" T.# T.set T.style [("width", "2px"), ("height", "600px")],
                                  UI.div T.#. "textArea" T.# T.set T.style [("width", "48%")
                                                                     , ("height", "450px")
                                                                     , ("border", "1px solid #000")
                                                                     , ("float", "right")]
                                  T.#+ [T.element codeArea]
                                  ]
                        , UI.div T.#. "forms" T.# T.set T.style [("width", "100%")] T.#+ [UI.div T.#. "transfForm"
                                                                                    T.# T.set T.style [("width", "48%"), ("float", "left")]
                                                                                     T.#+ [T.element transfForm]
                                                                          , UI.div T.#. "navigateAST"
                                                                                    T.# T.set T.style [("width", "48%"), ("float", "right")]
                                                                                     T.#+ [T.element navForm]
                                                                          ]
                                  ]
  -- bodyWindow         <- T.getBody window

  let

  -- Track all user input events
      codeAreaE           = T.filterE isCompileReady (UI.valueChange codeArea)
      mouseUpE            = UI.mouseup diagWindow
      mousedownE          = UI.mousedown diagWindow
      mousemoveE          = UI.mousemove diagWindow
      upClickE            = UP    <$ UI.click upButton
      downClickE          = DOWN  <$ UI.click downButton
      leftClickE          = LEFT  <$ UI.click leftButton
      rightClickE         = RIGHT <$ UI.click rightButton
      -- splitClickE         = UI.click splitButton
      (splitInputE :: T.Event [(Int, String)])        = T.filterE (/= []) (readInt <$> UI.valueChange iterCountTxtfld)
      spaceE              = (==32) <$> UI.keydown codeArea

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

    -- merge mousedown event with drag event to avoid jumping when dragging resumes
      unionEventMvDown = T.unionWith const (fst <$> mouseInTupleE) mvPointsE

  bPrevMousePt                                     <- T.stepper origin unionEventMvDown

  let

  -- simulate dragging
      (translations       :: T.Event (V2 Double))                     = fmap (\prev cur -> cur .-. prev) bPrevMousePt T.<@> mvPointsE

  {- merge all possible edits to the diagram into one data type
  , run the edits and generate behavior of obtained simpleDiagram -}

      (formedSDdataE  :: T.Event SDdata)                              = mergeEvents codeAreaE translations mouseOutE upClickE downClickE leftClickE rightClickE splitInputE
      (simpleDiagramE :: T.Event ((GenZipper, T2 Double) -> (GenZipper, T2 Double)))         = T.filterJust (runSDdata <$> formedSDdataE)

  (simpleDiagramB'  :: T.Behavior (GenZipper, T2 Double))     <- T.accumB (SDZ (Pr SEmpty) (LAssignSDCtx [] TopGenCtx), mempty) simpleDiagramE
  testBehavior                                    <- T.stepper (V2 0.0 0.0) translations

  let
  -- render diagram
      simpleDiagramB = unZipGenZ <$> (cursify <$> simpleDiagramB')
      -- simpleDiagramB = unZipGenZ <$> simpleDiagramB'
      dTupleB        = makeDToDraw' <$> simpleDiagramB
      diagramStrB    = fmap (\(x, y, z) -> x) dTupleB
      transformB     = fmap (\(x, y, z) -> y) dTupleB
      q2DiagramB     = fmap (\(x, y, z) -> z) dTupleB
      codeAreaStrB   = (pprintProg <$> simpleDiagramB)


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


makeDToDraw' :: Prog -> (String, (T2 Double), (QDiagram SVG V2 Double Any))
makeDToDraw' pr = let code = interpProg M.empty pr # withEnvelope (square 10 :: Diagram B) in let (tr, svgStr) = renderedString' code in (parseSVG $ svgStr, tr, code)

tripleFst :: (a, b, c) -> a
tripleFst (a, b, c) = a


runSDdata :: SDdata -> Maybe ((GenZipper, T2 Double) -> (GenZipper, T2 Double))
runSDdata (FrmCode str)  = case myparse parseProg str of
  Right prog -> Just $ const (makeGenZipper prog)
  _          -> Nothing
runSDdata (DragCoord cd) = Just $ \zp@(gz, tr) -> case gz of
  (SDZ _ _)     -> refactorSDOnDrag tr cd zp
  (TransZ _ _)  -> reTransfromZOnDrag cd zp
  (IntZ _ _)    -> addToIntOnDrag cd zp
  (DoubleZ _ _) -> addToDoubleOnDrag cd zp
  _             -> zp

runSDdata (Click pt)     = Just $ \zp@(gz, tr) -> case gz of
  SDZ sd ctx -> editZipper (createNewDiagram (transform tr pt) sd) zp
  _          -> zp
runSDdata (Nav k)        = Just $ navigateProg k
runSDdata (Split n)      = Just $ splitLGenZipper [n]





newCircleCreation :: P2 Double -> SimpleDiagram -> SimpleDiagram
newCircleCreation pt (Pr SEmpty) = createNewCircle pt
newCircleCreation pt sd          = Atop (createNewCircle pt) sd

createNewDiagram :: P2 Double -> SimpleDiagram -> SimpleDiagram -> SimpleDiagram
createNewDiagram pt sd1 sd = case sd1 of
  Pr SEmpty -> createNewCircle pt
  _         -> Atop (T (Translate (pt .-. origin)) sd1) sd



attributeList :: [(String, String)] -> T.UI T.Element -> T.UI T.Element
{- give list of key, value pairs and where keys are html attributes
 for the element. -}
attributeList l u = foldr (\(attr, val) ui -> T.set (T.attr attr) val ui) u l

-- creating and new circle, to be used on click outside of diagram.
createNewCircle :: P2 Double -> SimpleDiagram
createNewCircle p = T (Translate (p .-. origin)) (Pr Circle)

-- merge all different kinds of edits to the diagram into one data type.
mergeEvents :: T.Event String -> T.Event (V2 Double) -> T.Event (P2 Double)
               -> T.Event DIRECTION -> T.Event DIRECTION -> T.Event DIRECTION -> T.Event DIRECTION -> T.Event [(Int, String)] -> T.Event SDdata
mergeEvents e1 e2 e3 e4 e5 e6 e7 e8 = head <$> T.unions [FrmCode <$> e1, DragCoord <$> e2, Click <$> e3, Nav <$> e4, Nav <$> e5, Nav <$> e6, Nav <$> e7, fmap (\l -> Split $ fst $ head l) e8]

-- turn returned coordinate into point
makePoint :: (Int, Int) -> P2 Double
makePoint (x, y) = p2 (fromIntegral x, fromIntegral y)



refactorSDOnDrag :: T2 Double -> V2 Double -> (GenZipper, T2 Double) -> (GenZipper, T2 Double)
refactorSDOnDrag tr v gz = editZipper (refactorTree' (transform (inv tr) v)) gz


refactorTree' :: V2 Double -> SimpleDiagram -> SimpleDiagram
refactorTree'  p (T (Translate v) sd)          = T (Translate (p ^+^ v)) sd
refactorTree'  v  sd                       = T (Translate v) sd

reTransfromTree :: V2 Double -> TransformationEdit -> TransformationEdit
reTransfromTree v (Scale d)      = Scale ((norm v) + d)
reTransfromTree v (Translate v1) = Translate (v ^+^ v1)
reTransfromTree v (Rotate a)     = Rotate (a + 20*(norm v))

addToIntInTree :: V2 Double -> Int -> Int
addToIntInTree v n = n + ceiling (norm v)

addToDoubleInTree :: V2 Double -> Double -> Double
addToDoubleInTree v d = d + (norm v)

addToIntOnDrag :: V2 Double -> (GenZipper, T2 Double) -> (GenZipper, T2 Double)
addToIntOnDrag v z = editZipper (addToIntInTree v) z

addToDoubleOnDrag :: V2 Double -> (GenZipper, T2 Double) -> (GenZipper, T2 Double)
addToDoubleOnDrag v z = editZipper (addToDoubleInTree v) z

reTransfromZOnDrag :: V2 Double -> (GenZipper, T2 Double) -> (GenZipper, T2 Double)
reTransfromZOnDrag v z = editZipper (reTransfromTree v) z



splitTransFormHelper :: Int -> TransformationEdit -> TransformationEdit
splitTransFormHelper n (Scale d) = Scale (d^n)
splitTransFormHelper n (Translate (V2 v1 v2)) = let m = fromIntegral n in Translate (V2 (v1*m) (v2*m))
splitTransFormHelper n (Rotate a) = let m = fromIntegral n in Rotate (a*m)

splitTree' :: Int -> SimpleDiagram -> SimpleDiagram
splitTree' n zp@(Iterate m tra t sd) = if n `elem` t then zp else Atop (T (splitTransFormHelper (n - 1) tra) sd) (Iterate m tra ((n : t)) sd)
splitTree' n sd                        = sd

splitTree :: [Int] -> SimpleDiagram -> SimpleDiagram
splitTree l sd = foldr (\n sd1 -> splitTree' n sd1) sd l


isCreatedVarFormat :: String -> Maybe Integer
isCreatedVarFormat ('x':xs) = readMaybe xs
isCreatedVarFormat _        = Nothing




generateNewName :: [Assign] -> String
generateNewName [] = "x0"
generateNewName l = let l1 = catMaybes $ fmap (\(s, sd) -> isCreatedVarFormat s) l in
                        case l1 of
                            (x:xs) -> "x" ++ show ((maximum l1) + 1)
                            []     -> "x0"



splitGenZipper :: Int -> (GenZipper, T2 Double) -> (GenZipper, T2 Double)
splitGenZipper n z@(gz, tr) = case gz of
  SDZ (Iterate m tra t sd) lassctx@(LAssignSDCtx l ctx) -> case sd of
                                  Var var -> (SDZ (Atop (T (splitTransFormHelper (n - 1) tra) sd) (Iterate m tra ((n: t)) sd)) lassctx, tr)
                                  _       -> if n `elem` t then (gz, tr)
                                               else let name = generateNewName l
                                                      in (SDZ (Atop (T (splitTransFormHelper (n - 1) tra) (Var name)) (Iterate m tra (n:t) (Var name))) (LAssignSDCtx (l ++ [(name, sd)]) ctx), tr)
  pzi                                                   -> (pzi, tr)




splitLGenZipper :: [Int] -> (GenZipper, T2 Double) -> (GenZipper, T2 Double)
splitLGenZipper l gz = foldr (\n gz1 -> splitGenZipper n gz1) gz l


navigateProg :: DIRECTION -> (GenZipper, T2 Double) -> (GenZipper, T2 Double)
navigateProg k gz
              |k == DOWN  = downGenZ gz
              |k == UP    = upGenZ gz
              |k == LEFT  = leftGenZ gz
              |k == RIGHT = rightGenZ gz
              |otherwise = gz


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
pprintTransfromEdit (CurTr tr) = "+ " ++ pprintTransfromEdit tr
pprintTransfromEdit (Scale d) = "scale " ++ show d
pprintTransfromEdit (Translate v) = "translate " ++ pprintVec v
pprintTransfromEdit (Rotate a)    = "rotate " ++ show a

pprintTree ::  SimpleDiagram -> String
pprintTree = pprintTree' 0

replicateSp :: Int -> String
replicateSp n = foldr (++) [] (replicate n "\t")


pprintTree' ::  Int -> SimpleDiagram -> String
pprintTree' n (Var s)            = replicateSp n ++ s
pprintTree' n (Let s sd1 sd2) = replicateSp n ++ "let " ++ s ++ " = \n" ++ pprintTree' (n + 1) sd1 ++ " in \n" ++ pprintTree' (n + 1) sd2
pprintTree' n (Pr d)             = replicateSp n ++ (pprintPrim d)
pprintTree' n (T tra sd)         = replicateSp n ++ (pprintTransfromEdit tra) ++ "\n" ++  pprintTree' (n + 1) sd
pprintTree' n (Atop sdl sdr)     = replicateSp n ++"atop " ++ "\n"  ++ pprintTree' (n + 1) sdl  ++ "\n"  ++  pprintTree' (n + 1) sdr
pprintTree' n (Iterate m tra may sd) = case may of
  [] -> replicateSp n ++ "iterate " ++ show m ++ " " ++  pprintTransfromEdit tra  ++ "\n"  ++  pprintTree' (n + 1) sd
  t  -> replicateSp n ++ "iterate " ++ show m ++ " " ++  pprintTransfromEdit tra  ++ " [/" ++ pprintNList t ++ "\n"  ++  pprintTree' (n + 1) sd
pprintTree' n (Cursor sd)        = case sd of
  (Pr SEmpty)   -> pprintTree' n sd
  _             -> "==> " ++ pprintTree' n sd

pprintNList :: [Int] -> String
pprintNList [] = ""
pprintNList [x] = show x ++ "]"
pprintNList (x:xs) = show x ++ ", " ++ pprintNList xs


readInt :: String -> [(Int, String)]
readInt s = (reads s :: [(Int, String)])

pprintAssign :: Assign -> String
pprintAssign (s, sd) = s ++ " = " ++ pprintTree sd ++ ";"


pprintLAssign :: [Assign] -> String
pprintLAssign [] = ""
pprintLAssign l = unlines (fmap pprintAssign (reverse $ tail $ reverse l)) ++ ((\(s, sd) -> s ++ " = " ++ pprintTree sd) (last l))


pprintProg :: Prog -> String
pprintProg (PVars l)      = "# \n" ++ pprintLAssign l ++ " #"
pprintProg (PSdia sd)     = pprintTree sd
pprintProg (ProgVS l sd)  = "# \n" ++ pprintLAssign l ++ "\n # \n" ++ pprintTree sd
