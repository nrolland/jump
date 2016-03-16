{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
-- | Monad transformers and classes
module Jump.Trans
    ( WriterT
    , runWriterT
    , execWriterT
    ) where

import Start

import Control.Monad
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix

instance (Monad m, Monoid w) => MonadWriter w (WriterT w m) where
    writer (x, w2) = WriterT $ \w1 -> return (x, w1 <> w2)
    {-# INLINE writer #-}

    tell w = writer ((), w)
    {-# INLINE tell #-}

    listen m = WriterT $ \w' -> do
        (a, w) <- unWriterT m w'
        return ((a, w), w)
    {-# INLINE listen #-}

    pass m = WriterT $ \w' -> do
        ((a, f), w) <- unWriterT m w'
        return (a, f w)
    {-# INLINE pass #-}

-- ---------------------------------------------------------------------------
-- | A writer monad parameterized by:
--
--   * @w@ - the output to accumulate.
--
--   * @m@ - The inner monad.
--
-- The 'return' function produces the output 'mempty', while @>>=@
-- combines the outputs of the subcomputations using 'mappend'.
--
-- Unlike the @WriterT@ implementations in transformers, this implementation
-- does not leak memory. See:
-- https://mail.haskell.org/pipermail/libraries/2012-October/018599.html
newtype WriterT w m a = WriterT { unWriterT :: w -> m (a, w) }

runWriterT :: Monoid w => WriterT w m a -> m (a, w)
runWriterT f = unWriterT f mempty
{-# INLINE runWriterT #-}

{- FIXME
instance (Eq w, Eq1 m) => Eq1 (WriterT w m) where
    liftEq eq (WriterT m1) (WriterT m2) = liftEq (liftEq2 eq (==)) m1 m2
    {-# INLINE liftEq #-}

instance (Ord w, Ord1 m) => Ord1 (WriterT w m) where
    liftCompare comp (WriterT m1) (WriterT m2) =
        liftCompare (liftCompare2 comp compare) m1 m2
    {-# INLINE liftCompare #-}
-}

{- FIXME
instance (Read w, Read1 m) => Read1 (WriterT w m) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp' rl') "WriterT" WriterT
      where
        rp' = liftReadsPrec2 rp rl readsPrec readList
        rl' = liftReadList2 rp rl readsPrec readList

instance (Show w, Show1 m) => Show1 (WriterT w m) where
    liftShowsPrec sp sl d (WriterT m) =
        showsUnaryWith (liftShowsPrec sp' sl') "WriterT" d m
      where
        sp' = liftShowsPrec2 sp sl showsPrec showList
        sl' = liftShowList2 sp sl showsPrec showList
-}

{- FIXME
instance (Eq w, Eq1 m, Eq a) => Eq (WriterT w m a) where (==) = eq1
instance (Ord w, Ord1 m, Ord a) => Ord (WriterT w m a) where compare = compare1
instance (Read w, Read1 m, Read a) => Read (WriterT w m a) where
    readsPrec = readsPrec1
instance (Show w, Show1 m, Show a) => Show (WriterT w m a) where
    showsPrec = showsPrec1
-}

-- | Extract the output from a writer computation.
--
-- * @'execWriterT' m = 'liftM' 'snd' ('runWriterT' m)@
execWriterT :: (Monoid w, Monad m) => WriterT w m a -> m w
execWriterT m = do
    (_, w) <- runWriterT m
    return w
{-# INLINE execWriterT #-}

instance (Functor m) => Functor (WriterT w m) where
    fmap f (WriterT g) = WriterT $ \w -> first f <$> g w
    {-# INLINE fmap #-}

{- FIXME
instance (Foldable f, Monoid w) => Foldable (WriterT w f) where
    foldMap f = foldMap (f . fst) . runWriterT
    {-# INLINE foldMap #-}

instance (Traversable f, Monoid w) => Traversable (WriterT w f) where
    traverse f = fmap WriterT . traverse f' . runWriterT where
       f' (a, b) = fmap (\ c -> (c, b)) (f a)
    {-# INLINE traverse #-}
-}

instance (Monoid w, Applicative m) => Applicative (WriterT w m) where
    pure a  = WriterT $ \w -> pure (a, w)
    {-# INLINE pure #-}
    f <*> v = WriterT $ \w -> liftA2 k (unWriterT f w) (unWriterT v w)
      where k (a, w) (b, w') = (a b, w `mappend` w')
    {-# INLINE (<*>) #-}

instance (Monoid w, Alternative m) => Alternative (WriterT w m) where
    empty   = WriterT (const empty)
    {-# INLINE empty #-}
    m <|> n = WriterT $ \w -> unWriterT m w <|> unWriterT n w
    {-# INLINE (<|>) #-}

instance (Monoid w, Monad m) => Monad (WriterT w m) where
#if !(MIN_VERSION_base(4,8,0))
    return = pure
    {-# INLINE return #-}
#endif
    m >>= k  = WriterT $ \w0 -> do
        (a, w1) <- unWriterT m w0
        (b, w2) <- unWriterT (k a) w1
        return (b, w2)
    {-# INLINE (>>=) #-}
    fail msg = WriterT $ \_ -> fail msg
    {-# INLINE fail #-}
    (>>) = (*>)
    {-# INLINE (>>) #-}

instance (Monoid w, Fail.MonadFail m) => Fail.MonadFail (WriterT w m) where
    fail msg = WriterT $ \_ -> Fail.fail msg
    {-# INLINE fail #-}

instance (Monoid w, MonadPlus m) => MonadPlus (WriterT w m) where
    mzero = empty
    {-# INLINE mzero #-}
    mplus = (<|>)
    {-# INLINE mplus #-}

instance (Monoid w, MonadFix m) => MonadFix (WriterT w m) where
    mfix f = WriterT $ \ w -> mfix $ \ ~(a, _) -> unWriterT (f a) w
    {-# INLINE mfix #-}

instance (Monoid w) => MonadTrans (WriterT w) where
    lift m = WriterT $ \w0 -> do
        a <- m
        return (a, w0)
    {-# INLINE lift #-}

instance (Monoid w, MonadIO m) => MonadIO (WriterT w m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

{- FIXME
-- | Lift a @callCC@ operation to the new monad.
liftCallCC :: (Monoid w) => CallCC m (a,w) (b,w) -> CallCC (WriterT w m) a b
liftCallCC callCC f = WriterT $
    callCC $ \ c ->
    runWriterT (f (\ a -> WriterT $ c (a, mempty)))
{-# INLINE liftCallCC #-}

-- | Lift a @catchE@ operation to the new monad.
liftCatch :: Monoid w => Catch e m (a,w) -> Catch e (WriterT w m) a
liftCatch catchE m h =
    WriterT $ runWriterT m `catchE` \ e -> runWriterT (h e)
{-# INLINE liftCatch #-}
-}
