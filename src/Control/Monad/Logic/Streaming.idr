module Control.Monad.Logic.Streaming

import Control.Monad.Identity
import Control.Monad.Trans
import Streaming
import public Control.Monad.Logic.Interface

export
data LogicT : (Type -> Type) -> Type -> Type where
  MkLogicT : Stream (Of a) m () -> LogicT m a

public export
Logic : Type -> Type
Logic = LogicT Identity

export
observe : LogicT m a -> Stream (Of a) m ()
observe (MkLogicT z) = z

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
