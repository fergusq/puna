module Coroutine where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.State

-- CoroutineT monad

data CoroutineT s m t
    = Return t
    | Yield s (CoroutineT s m t)
    | YieldT (m (CoroutineT s m t))

instance (Show s, Show t) => Show (CoroutineT s m t) where
    show (Return x) = "Return (" ++ show x ++ ")"
    show (Yield s k) = "Yield " ++ show s ++ " (" ++ show k ++ ")"
    show (YieldT m) = "YieldT _"

instance (Monad m) => Monad (CoroutineT s m) where
    return            = Return
    (Return x)  >>= f = f x
    (Yield x k) >>= f = Yield x (k >>= f)
    (YieldT i)  >>= f = YieldT $ do x <- i
                                    return $ (x >>= f)

instance (Monad m) => Functor (CoroutineT s m) where
    fmap = liftM

instance (Monad m) => Applicative (CoroutineT s m) where
    pure = return
    (<*>) = ap

instance MonadTrans (CoroutineT s) where
    lift i = YieldT (i >>= \x -> return (return x))

-- push and pull

push :: (Monad m) => s -> CoroutineT s m ()
push = flip Yield (return ())

pull :: (Monad m) => CoroutineT s m t -> m (Maybe (s, CoroutineT s m t))
pull (Return t) = return Nothing
pull (Yield x k) = return $ Just (x, k)
pull (YieldT i) = do k <- i
                     pull k

-- pipes and streams

data StreamState s m = CR (CoroutineT s m (StreamState s m))
                     | NoCR

type StreamT s m r = StateT (StreamState s m) (CoroutineT s m) r

pipe :: (Monad m) => CoroutineT s m (StreamState s m) -> StreamT s m () -> CoroutineT s m (StreamState s m)
pipe input f = do (_, s) <- runStateT f (CR input)
                  return s

(|>) :: (Monad m) => CoroutineT s m (StreamState s m) -> StreamT s m () -> CoroutineT s m (StreamState s m)
(|>) = pipe

pushPipe :: (Monad m) => s -> StreamT s m ()
pushPipe = lift . push

pullPipe :: (Monad m) => StreamT s m (Maybe s)
pullPipe = do (CR l) <- get
              p <- (lift.lift) $ pull l
              case p of
                  Nothing -> return Nothing
                  Just (x, l') -> do put (CR l')
                                     return (Just x)

pipePipe :: (Monad m) => [StreamT s m ()] -> StreamT s m ()
pipePipe []     = identityPipe
pipePipe (s:ss) = do (CR input) <- get
                     (CR input') <- lift $ (input |> s) |> (pipePipe ss)
                     state <- lift $ input'
                     put state

identityPipe :: (Monad m) => StreamT s m ()
identityPipe = do x <- pullPipe
                  case x of
                      Just v -> pushPipe v >> identityPipe
                      Nothing -> return ()


