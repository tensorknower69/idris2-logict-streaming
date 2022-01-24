module Control.Monad.Logic.Streaming

import Control.Monad.Error.Interface
import Control.Monad.Identity
import Control.Monad.Reader.Interface
import Control.Monad.State.Interface
import Control.Monad.Trans
import Streaming
import public Control.Monad.Logic.Interface

public export
record LogicT (m : Type -> Type) (a : Type) where
  constructor MkLogicT
  observe : Stream (Of a) m ()

export
Monad m => Functor (LogicT m) where
  map f z = MkLogicT $ maps f $ observe z

export
Monad m => Applicative (LogicT m) where
  pure x = MkLogicT $ yield x
  fz <*> xz = MkLogicT $ for (observe fz) $ \f => maps f (observe xz)

export
Monad m => Monad (LogicT m) where
  act >>= react = assert_total $ MkLogicT $ for (observe act) $ \x => observe (react x)

export
Monad m => Alternative (LogicT m) where
  empty = MkLogicT $ pure ()
  xz <|> yz = MkLogicT $ observe xz >> observe yz

export
MonadTrans LogicT where
  lift x = MkLogicT (lift x >>= yield)

export
Monad m => MonadLogic (LogicT m) where
  split z = pure $ case !(lift $ next $ observe z) of
    Left () => Nothing
    Right (x, xs) => Just (x, MkLogicT xs)

export
HasIO m => HasIO (LogicT m) where
  liftIO = lift . liftIO

export
MonadError e m => MonadError e (LogicT m) where
  throwError e = lift $ throwError e
  z `catchError` f = MkLogicT $ observe z `catchError` observe . f

export
MonadState s m => MonadState s (LogicT m) where
  put = lift . put
  get = lift get
  state f = lift (state f)

export
MonadReader s m => MonadReader s (LogicT m) where
  ask = lift ask
  local f z = MkLogicT $ local f (observe z)
