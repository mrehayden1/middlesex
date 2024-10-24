module App.UI (
  UI(..),
  UIElemID,

  UIBuilder(..),
  UIElemEvents(..),
  UIElemEventType(..),

  UIBuilderT,
  runUiBuilderT,

  UIEnv(..),
  UIEvents(..),

  banner,
  button,
  card
) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.IORef
import Data.Map
import Linear
import Linear.Affine
import Reflex

import App.Graphics
import App.Graphics.Text
import App.Graphics.UI


class UIBuilder t os m | m -> t, m -> os where
  elem'    :: (UIElemID -> [UIElem os] -> UIElem os) -> m a -> m (a, UIElemEvents t)
  uiEvents :: m (UIEvents t)
  text'    :: TypefaceIx -> PixelSize -> Colour -> String -> m (SDFText os)

data UIElemEvents t = UIElemEvents {
    click :: Event t (),
    mouseOut :: Event t (),
    mouseOver :: Event t ()
  }

data UIElemEventType = UIEventClick | UIEventMouseOut | UIEventMouseOver
  deriving (Eq, Ord)

newtype UIBuilderT t os m a = UIBuilderT {
    unUiBuilderT :: ReaderT (IORef UIElemID, UIEnv t os) (StateT (UIState t os) m) a
  }
 deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

type UIState t os = (Behavior t [UIElem os], Behavior t TriggerMap)

type TriggerMap = Map (UIElemEventType, UIElemID) (() -> IO ())

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
  -> m (a, Behavior t [UIElem os], Behavior t TriggerMap)
runUiBuilderT e m = do
  idRef <- liftIO $ newIORef 1
  (a, (elems', triggerMap)) <-
    flip runStateT (pure [], pure empty) . flip runReaderT (idRef, e)
      . unUiBuilderT $ m
  return (a, elems', triggerMap)


-- Convenience UI builders for specific UI elements
banner :: (UIBuilder t os m, Monad m) => String -> m ()
banner str = do
  text <- fmap UIText . text' typefaceIxLabel 320 (V4 0 0 0 1) $ str
  let el = UINineSlice UIMaterialBanner True False
  void . elem' (const . flip el [text]) $ pure ()

button :: (UIBuilder t os m, Monad m) => String -> m (UIElemEvents t)
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

    -- This isn't atomic - does that matter?
    i <- liftIO $ readIORef idRef
    liftIO $ writeIORef idRef . succ $ i

    (clickE, clickTrigger) <- newTriggerEvent
    (mouseOutE, mouseOutTrigger) <- newTriggerEvent
    (mouseOverE, mouseOverTrigger) <- newTriggerEvent

    let triggers = fromList [
            ((UIEventClick    , i), clickTrigger    ),
            ((UIEventMouseOut , i), mouseOutTrigger ),
            ((UIEventMouseOver, i), mouseOverTrigger)
          ]
        events = UIElemEvents {
            click = clickE,
            mouseOut = mouseOutE,
            mouseOver = mouseOverE
          }

    (a, (childElems, childTriggers)) <- lift
      . flip runStateT (pure [], pure empty)
      . flip runReaderT (idRef, env)
      . unUiBuilderT
      $ children

    UIBuilderT . modify $ \(els, ts) ->
      let elems' = (:) <$> fmap (el i) childElems <*> els
          triggers' = (<>) <$> fmap (triggers <>) childTriggers <*> ts
      in (elems', triggers')

    return (a, events)

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

instance (Adjustable t m, MonadHold t m) => Adjustable t (UIBuilderT t os m) where
  runWithReplace a0 a' = do
    e <- UIBuilderT ask
    (result0, result') <- lift $
      runWithReplace
        (flip runStateT (pure [], pure empty) . flip runReaderT e . unUiBuilderT  $  a0)
        (flip runStateT (pure [], pure empty) . flip runReaderT e . unUiBuilderT <$> a')
    o <- hold (snd result0) $ fmapCheap snd result'
    let elems' = fst =<< o
        triggers = snd =<< o
    UIBuilderT . modify . bimap (liftM2 (<>) elems') $ liftM2 (<>) triggers
    return (fst result0, fmapCheap fst result')
