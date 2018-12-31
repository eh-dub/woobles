{-# LANGUAGE FlexibleContexts #-}

module MyMonadState where

import qualified Control.Monad.State as S
import Types
import Diagrams.Prelude
import Diagrams.Backend.Cairo

put :: S.MonadState MyDiagram m => Diagram B -> m ()
put = S.put . MyDiagram 

get :: S.MonadState MyDiagram m => m (Diagram B)
get = fmap unMyDiagram S.get

modify :: S.MonadState MyDiagram m => (Diagram B -> Diagram B) -> m ()
modify f = S.modify $ MyDiagram . f . unMyDiagram
