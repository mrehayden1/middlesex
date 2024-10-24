module App.UI (
  UI(..),

  UIBuilder(..),

  UIEvents(..),

  UIBuilderT,
  runUiBuilderT,

  UIEnv(..),

  banner,
  button,
  card
) where

import Control.Monad.Reader
import Control.Monad.State
import Data.IORef
import Data.Map
import Linear
import Linear.Affine
import Reflex

import App.Graphics
import App.Graphics.Text
import App.Graphics.UI


class UIBuilder t os m | m -> t, m -> os where
  elem'    :: (UIElemID -> [UIElem os] -> UIElem os) -> m a -> m (a, Event t ())
  uiEvents :: m (UIEvents t)
  text'    :: TypefaceIx -> PixelSize -> Colour -> String -> m (SDFText os)


newtype UIBuilderT t os m a = UIBuilderT {
    unUiBuilderT :: ReaderT (IORef UIElemID, UIEnv t os) (StateT (UIState t os) m) a
  }
 deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

type UIState t os = ([Behavior t (UIElem os)], TriggerMap)

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

runUiBuilderT :: (Reflex t, MonadIO m)
  => UIEnv t os
  -> UIBuilderT t os m a
  -> m (a, Behavior t [UIElem os], TriggerMap)
runUiBuilderT e m = do
  idRef <- liftIO $ newIORef 1
  (a, (bs, triggerMap)) <-
    flip runStateT ([], empty) . flip runReaderT (idRef, e) . unUiBuilderT $ m
  return (a, sequence bs, triggerMap)


-- Convenience UI builders for specific UI elements
banner :: (UIBuilder t os m, Monad m) => String -> m ()
banner str = do
  text <- fmap UIText . text' typefaceIxLabel 320 (V4 0 0 0 1) $ str
  let el = UINineSlice UIMaterialBanner True False
  void . elem' (const . flip el [text]) $ pure ()

button :: (UIBuilder t os m, Monad m) => String -> m (Event t ())
button str = do
  text <- fmap UIText . text' typefaceIxLabel 128 (V4 1 1 1 1) $ str
  fmap snd
    . elem' (const . flip (UINineSlice UIMaterialButton True False) [text])
    $ pure ()

card :: Functor m => UIBuilder t os m => m a -> m a
card children = do
  fmap fst . elem' (UINineSlice UIMaterialCard True True) $ children


-- Instances
instance (Reflex t, TriggerEvent t m, MonadIO m) =>
    UIBuilder t os (UIBuilderT t os m) where
  elem' el children = do
    (idRef, env) <- UIBuilderT ask
    (clickE, clickTrigger) <- newTriggerEvent

    -- This isn't atomic - does that matter?
    i <- liftIO $ readIORef idRef
    liftIO $ writeIORef idRef . succ $ i

    (a, (childElems, childTriggers)) <- lift
        . flip runStateT ([], empty)
        . flip runReaderT (idRef, env)
        . unUiBuilderT
        $ children

    UIBuilderT . modify $ \(els, ts) ->
      let elems' = (fmap (el i) . sequence $ childElems) : els
          triggers = insert i clickTrigger ts <> childTriggers
      in (elems', triggers)

    return (a, clickE)

  text' faceIx sz colour str = do
    font <- UIBuilderT $ asks (uiEnvFont . snd)
    return . uiSdfText font faceIx sz colour $ str

  uiEvents = UIBuilderT $ asks (uiEnvEvents . snd)

instance (Monad m, UIBuilder t os m) => UIBuilder t os (ReaderT e m) where
  elem' e cs = lift . elem' e . runReaderT cs =<< ask
  text' t c sz = lift . text' t c sz
  uiEvents = lift uiEvents


instance MonadTrans (UIBuilderT t os) where
  lift = UIBuilderT . lift . lift

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
