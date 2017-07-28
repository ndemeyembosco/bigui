# BIGUIEL

A functionally reactive program that lets users bi-directionally interact with diagrams. i.e a user is allowed to write code that generates a certain diagram while at the same time, he/she is allowed to manipulate the diagram, by say dragging, updating the same code he/she wrote in real time.


![alt-text](giphy.gif)

![alt-text](giphy-downsized-large.gif)

---

The following is an explanation of how the program is built using the functional reactive programming library [**Reactive.Threepenny**](https://hackage.haskell.org/package/threepenny-gui-0.8.0.1/docs/Reactive-Threepenny.html) as well as the [**diagrams**](http://projects.haskell.org/diagrams/) library.

As of now, this project uses a tiny language defined as follows to declaratively define the diagrams

```haskell

type Assign = (String, SimpleDiagram)

data Prog where
  PVars  :: [Assign]      -> Prog
  PSdia  :: SimpleDiagram -> Prog
  ProgVS :: [Assign]      -> SimpleDiagram -> Prog
  deriving (Show)

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
```
----

### Program Structure/Design

First and foremost, we need a way to model the different kinds of
edits that a user can make both the diagram. To do this, diagram data, data type is created.

```haskell
data SDdata where
  FrmCode   :: String   -> SDdata
  DragCoord :: V2 Double -> SDdata
  Click     :: P2 Double -> SDdata
  Nav       :: DIRECTION -> SDdata
  Split     :: Int       -> SDdata
  deriving (Show)
```

The names of constructors and what they correspond to are as follows. The **FrmCode str** constructor represents the changes made in the text editor, the **DragCoord v** constructor represents the vector obtained by subtracting the previous position of the mouse from the current position of the mouse while dragging, the **Click pt** constructor corresponds to the point clicked inside the diagram side of the GUI, the **Nav dir** constructor represents the action of navigating through the syntax tree using control keys(or in this version, control buttons), while the **Split n** constructor corresponds to "splitting" a diagram from an iterate node and letting it be its own node in the tree while leaving all the other nodes still "connected".


In order to construct this data type, a function called **mergeEvent** whose type signature is as follows, gets used.
```haskell
mergeEvents :: T.Event String -> T.Event (V2 Double) -> T.Event (P2 Double)
               -> T.Event DIRECTION -> T.Event DIRECTION -> T.Event DIRECTION -> T.Event DIRECTION -> T.Event [(Int, String)] -> T.Event SDdata
```
The first **Event String** are the events corresponding to changing the contents of the text editor, and all the other events are completely analogous to their **SDdata** corresponding constructors.

In order to emulate the behavior that a certain part of the syntax tree is the current focus, which is useful for operations like *navigate*, a [zipper data-structure](https://en.wikibooks.org/wiki/Haskell/Zippers) is used. This zipper is defined as follows:

```haskell
data GenZipper where
  TopGenZ     :: GenZipper
  LAssignGenZ :: [Assign]            -> LAssignGenCtx -> GenZipper
  AssignZ     :: Assign              -> AssignCtx  -> GenZipper
  SDZ         :: SimpleDiagram       -> SDCtx      -> GenZipper
  DoubleZ     :: Double              -> DoubleCtx  -> GenZipper
  IntZ        :: Int                 -> IntCtx     -> GenZipper
  TransZ      :: TransformationEdit  -> TransCtx   -> GenZipper
  VarZ        :: String              -> VarCtx     -> GenZipper
  deriving Show
```

Each constructor corresponds to a different type of node(these are collectively connected with instances of the `Modifyable a` type class) that we might be focused on in the syntax-tree. This allows us to define different actions/behaviors on the diagram when different types of nodes are focused on. For example, if a user is focused on a translate node and starts dragging the diagram, the diagram in question is going to move in the direction of the drag. Similarly, if a user is focused on a scale node and does the same action(i.e dragging), the diagram is going to become bigger instead.

The data structure above is defined together with a function **editZipper**
with type signature `editZipper :: Modifyable a => (a -> a) -> (GenZipper, T2 Double) -> (GenZipper, T2 Double)`. As a matter of fact, this is the function used to define a **Modifyable** instance, the  `T2 Double` represents the current transformation being applied to section of the syntax currently focused on. What this allows us to do is to define different functions each one of type `f :: a -> a` where `a` is different type we might be focused on in the zipper. Thus, like stated in the paragraph above, allowing us,  for example, to scale when focused on a scale node, add when focused on a double or translate when focused on a diagram node, all while responding to the same action of dragging.

However, we still have not explained how we go from having, to use **Functional Reactive Programming** terminology, events of type `Event SDdata` to producing a behavior of type  `Behavior GenZipper`.

In order to do this, first, a function **runSDdata** with type signature `runSDdata :: SDdata -> Maybe ((GenZipper, T2 Double) -> (GenZipper, T2 Double))` is used. As one can probably guess, it takes a zipper and produces another zipper depending on what type of edit was made in the GUI. Second, we the user the `accumB :: accumB :: MonadIO m => a -> Event (a -> a) -> m (Behavior a)` function provided by the **Reactive.Threepenny** library to produce `Behavior (GenZipper, T2 Double)`.


Finally, we use/`fmap` another function `unZipGenZ :: (GenZipper, T2 Double)-> Prog` to unzip the zipper and produce a behavior of program data type.
```haskell
data Prog where
  PVars  :: [Assign]      -> Prog
  PSdia  :: SimpleDiagram -> Prog
  ProgVS :: [Assign]      -> SimpleDiagram -> Prog
  deriving (Show)
```

This then gets run by fmapping `makeDToDraw' :: Prog -> (String, (T2 Double), (QDiagram SVG V2 Double Any))`. This allows us to break `Behavior Prog` into three behaviors: one for the code that gets sunk back into the text editor, one for the transformation whose inverse allows us to correctly track when points on the diagram are cliked and act accordingly, and one for the diagram itself which allows to sample points clicked as either falling inside the diagram or not in case we needed to use that. (This sampling technique was used in the very first version of this file before we had thought of using  the zippper data structure).

After the `Behavior String` obtained from fmapping `makeDToDraw` onto `Behavior Prog` is sank back into text editor, we once again start tracking changes and constructing `SDdata` appropriately, and the process starts all over again. (a.k.a the RecursiveDo's `mdo` does its job).
