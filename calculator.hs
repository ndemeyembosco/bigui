import Control.Monad
import Data.Functor
import Data.List
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


main :: IO ()
main = do
  startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = void $ do
  return window # set title "calculator"
  inputw <- UI.input # set (attr "placeholder") "0"
  myGrid <- grid $ [[element inputw], [column makeButtons]]
  getBody window #+ [element myGrid]


  bs <- getElementsByTagName window "button"
  -- liftIO $ print $ length bs
  forM_ bs $ \b -> linkButton b inputw



makeButton :: Int -> UI Element
makeButton n = UI.button # set text (show n) # set value (show n)


makeButtons :: [UI Element]
makeButtons = map (\g -> row g) (map (\l1 -> map makeButton l1) [[1,2,3], [4,5,6],[7,8,9]])

linkButton :: Element -> Element -> UI ()
linkButton b inp = on UI.click b $ \_ -> do
  s <- get UI.value b
  liftIO $ print s
  element inp #  set value s
