module App.UI (
  UI(..),

  UIElem(..),
  UIBuilder(..),

  UIEvents(..),

  UIBuilderT,

  runUiBuilderT
) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Map
import Linear
import Linear.Affine
import Reflex

import App.Graphics
import App.Graphics.UI

class UIBuilder t m | m -> t where
  button :: String -> m (Event t ())
  card :: m a -> m a
  unhandledEvents :: m (UIEvents t)

type UIState = (Word, [UIElem], TriggerMap)

type TriggerMap = Map Word (() -> IO ())

data UIEvents t = UIEvents {
    eCursorPos :: Event t (Point V2 Int), -- Point in screen space (left-handed)
    eKey :: Event t (Key, KeyState),
    eMouseButton :: Event t (MouseButton, MouseButtonState)
  }

newtype UIBuilderT t m a = UIBuilderT {
    unUiBuilderT :: ReaderT (UIEvents t) (StateT UIState m) a
  }
 deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

runUiBuilderT :: Monad m
  => UIEvents t
  -> UIBuilderT t m a
  -> m (a, [UIElem], TriggerMap)
runUiBuilderT e m = do
  (a, (_, es, triggerMap)) <-
    flip runStateT (1, [], empty) . flip runReaderT e . unUiBuilderT $ m
  return (a, reverse es, triggerMap)

instance (Reflex t, TriggerEvent t m) => UIBuilder t (UIBuilderT t m) where
  button text = do
    (i, _, _) <- UIBuilderT get
    let el = UIButton i text
    (clickE, clickTrigger) <- newTriggerEvent
    UIBuilderT . modify $ \(_, els, ts) ->
      (succ i, el:els, insert i clickTrigger ts)
    return clickE

  card children = do
    e <- UIBuilderT ask
    (i, _, _) <- UIBuilderT get

    (a, (i', childElems, childTriggers)) <- lift
        . flip runStateT (i, [], empty) . flip runReaderT e . unUiBuilderT
        $ children

    UIBuilderT $ modify (\(_, es, ts) ->
      (i', (UICard . UIColumn $ childElems) : es, ts <> childTriggers))

    return a

  unhandledEvents = UIBuilderT ask

instance MonadTrans (UIBuilderT t) where
  lift = UIBuilderT . lift . lift

instance (Monad m, UIBuilder t m) => UIBuilder t (ReaderT e m) where
  button = lift . button
  card cs = lift . card . runReaderT cs =<< ask
  unhandledEvents = lift unhandledEvents

instance MonadReader e m => MonadReader e (UIBuilderT t m) where
  ask = lift ask
  local f = UIBuilderT . ReaderT . (local f .) . runReaderT . unUiBuilderT

instance PerformEvent t m => PerformEvent t (UIBuilderT t m) where
  type Performable (UIBuilderT t m) = Performable m
  {-# INLINABLE performEvent_ #-}
  performEvent_ e = lift $ performEvent_ e
  {-# INLINABLE performEvent #-}
  performEvent e = lift $ performEvent e

instance MonadSample t m => MonadSample t (UIBuilderT t m) where
  {-# INLINABLE sample #-}
  sample = lift . sample

instance MonadHold t m => MonadHold t (UIBuilderT t m) where
  {-# INLINABLE hold #-}
  hold v0 v' = lift $ hold v0 v'
  {-# INLINABLE holdDyn #-}
  holdDyn v0 v' = lift $ holdDyn v0 v'
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 v' = lift $ holdIncremental v0 v'
  {-# INLINABLE buildDynamic #-}
  buildDynamic a0 = lift . buildDynamic a0
  {-# INLINABLE headE #-}
  headE = lift . headE

instance PostBuild t m => PostBuild t (UIBuilderT t m) where
  {-# INLINABLE getPostBuild #-}
  getPostBuild = lift getPostBuild

instance TriggerEvent t m => TriggerEvent t (UIBuilderT t m) where
  {-# INLINABLE newTriggerEvent #-}
  newTriggerEvent = lift newTriggerEvent
  {-# INLINABLE newTriggerEventWithOnComplete #-}
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  {-# INLINABLE newEventWithLazyTriggerWithOnComplete #-}
  newEventWithLazyTriggerWithOnComplete f =
    lift $ newEventWithLazyTriggerWithOnComplete f
