{-# LANGUAGE GeneralizedNewtypeDeriving  #-}

module Types where    

import Graphics.Rendering.Cairo

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.RWS.Strict (RWST (..))
import Control.Monad.Trans

data World = World 
  { worldWidth  :: Double
  , worldHeight :: Double
  , scaleFactor :: Double
  }
  
data MyState = MyState 

newtype App a = App {unApp :: RWST World () MyState Render a}
  deriving (Functor, Applicative, Monad, MonadIO
           ,MonadState MyState, MonadReader World)

runApp :: World -> MyState -> App a -> Render a
runApp world state sketch = do
  (a, _, _)<- runRWST (unApp sketch) world state 
  pure a

liftApp :: Render a -> App a
liftApp = App . lift

type Wobble = (Double, Double)
