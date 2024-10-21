module App.UI (
  UI(..),

  UIElem(..),
  UIBuilder(..),

  UIEvents(..),

  UIBuilderT,
  UIEnv(..),

  runUiBuilderT
) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Map
import Linear
import Linear.Affine
import Reflex

import App.Graphics
import App.Graphics.Text
import App.Graphics.UI

class UIBuilder t m | m -> t where
  button :: String -> m (Event t ())
  card :: m a -> m a
  uiEvents :: m (UIEvents t)

type UIState os = (UIElemID, [UIElem os], TriggerMap)

type TriggerMap = Map UIElemID (() -> IO ())

data UIEnv t os = UIEnv {
    uiEnvEvents :: UIEvents t,
    uiEnvFont :: Font os
  }

data UIEvents t = UIEvents {
    eCursorPos :: Event t (Point V2 Int), -- Point in screen space (left-handed)
    eUnhandledKey :: Event t (Key, KeyState),
    eUnhandledMouseButton :: Event t (MouseButton, MouseButtonState)
  }

newtype UIBuilderT t os m a = UIBuilderT {
    unUiBuilderT :: ReaderT (UIEnv t os) (StateT (UIState os) m) a
  }
 deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

runUiBuilderT :: MonadIO m
  => UIEnv t os
  -> UIBuilderT t os m a
  -> m (a, [UIElem os], TriggerMap)
runUiBuilderT e m = do
  (a, (_, es, triggerMap)) <-
    flip runStateT (1, [], empty) . flip runReaderT e . unUiBuilderT $ m
  return (a, reverse es, triggerMap)

instance (Reflex t, TriggerEvent t m) => UIBuilder t (UIBuilderT t os m) where
  button str = do
    (i, _, _) <- UIBuilderT get
    font <- UIBuilderT $ asks uiEnvFont
    let el = UIButton i . uiSdfText font typefaceIxLabel 128 (V4 1 1 1 1) $ str
    (clickE, clickTrigger) <- newTriggerEvent
    UIBuilderT . modify $ \(_, els, ts) ->
      (succ i, el:els, insert i clickTrigger ts)
    return clickE

  card children = do
    env <- UIBuilderT ask
    (i, _, _) <- UIBuilderT get

    (a, (i', childElems, childTriggers)) <- lift
        . flip runStateT (i, [], empty) . flip runReaderT env . unUiBuilderT
        $ children

    UIBuilderT $ modify (\(_, els, ts) ->
      let el = UICard . UIColumn . reverse $ childElems
      in (i', el:els, ts <> childTriggers))

    return a

  uiEvents = UIBuilderT $ asks uiEnvEvents

instance MonadTrans (UIBuilderT t os) where
  lift = UIBuilderT . lift . lift

instance (Monad m, UIBuilder t m) => UIBuilder t (ReaderT e m) where
  button = lift . button
  card cs = lift . card . runReaderT cs =<< ask
  uiEvents = lift uiEvents

instance MonadReader e m => MonadReader e (UIBuilderT t os m) where
  ask = lift ask
  local f = UIBuilderT . ReaderT . (local f .) . runReaderT . unUiBuilderT

instance PerformEvent t m => PerformEvent t (UIBuilderT t os m) where
  type Performable (UIBuilderT t os m) = Performable m
  {-# INLINABLE performEvent_ #-}
  performEvent_ e = lift $ performEvent_ e
  {-# INLINABLE performEvent #-}
  performEvent e = lift $ performEvent e

instance MonadSample t m => MonadSample t (UIBuilderT t os m) where
  {-# INLINABLE sample #-}
  sample = lift . sample

instance MonadHold t m => MonadHold t (UIBuilderT t os m) where
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

instance PostBuild t m => PostBuild t (UIBuilderT t os m) where
  {-# INLINABLE getPostBuild #-}
  getPostBuild = lift getPostBuild

instance TriggerEvent t m => TriggerEvent t (UIBuilderT t os m) where
  {-# INLINABLE newTriggerEvent #-}
  newTriggerEvent = lift newTriggerEvent
  {-# INLINABLE newTriggerEventWithOnComplete #-}
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  {-# INLINABLE newEventWithLazyTriggerWithOnComplete #-}
  newEventWithLazyTriggerWithOnComplete f =
    lift $ newEventWithLazyTriggerWithOnComplete f
