{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Types where    

import qualified Graphics.Rendering.Cairo as C

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.RWS.Strict (RWST (..), RWS (..), runRWS)
import Control.Monad.Trans

import Diagrams.Prelude
import Diagrams.Backend.Cairo

data World = World 
  { worldWidth  :: Double
  , worldHeight :: Double
  , scaleFactor :: Double
  }
  
data MyState = MyState 

newtype App a = App {unApp :: RWST World () MyState C.Render a}
  deriving (Functor, Applicative, Monad, MonadIO
           ,MonadState MyState, MonadReader World)

newtype DApp a = DApp {unDApp :: RWS World () (MyDiagram) a}
  deriving (Functor, Applicative, Monad, MonadState MyDiagram
           , MonadReader World)

newtype MyDiagram = MyDiagram {unMyDiagram :: Diagram B}
  deriving (Monoid, Semigroup)

runDApp :: World -> DApp a -> Diagram B
runDApp world sketch =
  let (a, s, _) = runRWS (unDApp sketch) world mempty
  in
    unMyDiagram s
    
-- liftDApp :: Diagram B -> DApp a
-- liftDApp 


runApp :: World -> MyState -> App a -> C.Render a
runApp world state sketch = do
  (a, _, _)<- runRWST (unApp sketch) world state 
  pure a

liftApp :: C.Render a -> App a
liftApp = App . lift

type Wobble = (Double, Double)
