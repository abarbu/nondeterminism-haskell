{-# LANGUAGE RankNTypes #-}

module Control.Monad.Amb where
import Control.Monad.Cont
import Control.Monad.State.Strict
import Control.Monad.Identity
import Data.Monoid

-- A computation whose current value is of type a which will
-- ultimately return a value of type r. The same type as ContT.
data AmbT r m a = AmbT { unAmbT ::
                            StateT (AmbT r m r) -- computation to run on failure
                            (ContT r            -- continuation captured when making nondeterministic choices
                             (StateT [r] m))    -- record keeping of solutions found so far
                            a }
type Amb r = AmbT r Identity

type AmbT' m a = forall r. AmbT r m a
type Amb' a = AmbT' Identity a

instance MonadTrans (AmbT r) where
    lift = AmbT . lift . lift . lift

instance (Monad m) => Monad (AmbT r m) where
    AmbT a >>= b = AmbT $ a >>= unAmbT . b
    return = AmbT . return

-- Internals

ambCC :: ((a -> AmbT r m a1) -> AmbT r m a) -> AmbT r m a
ambCC f = AmbT $ callCC $ \k -> unAmbT $ f $ AmbT . k

runAmbTI :: Monad m => AmbT a m a -> AmbT a m a -> m (a, [a])
runAmbTI (AmbT a) i = runStateT (runContT (evalStateT a i) return) []

runAmbT :: Monad m => AmbT t m t -> m (t, [t])
runAmbT a = runAmbTI a (error "top-level fail")

uponFailure :: Monad m => AmbT r m a -> AmbT r m ()
uponFailure f = do
  old <- AmbT get
  AmbT $ put (f >> old)

tellState :: (Monoid s, MonadState s m) => s -> m ()
tellState b = do
  a <- get
  put $ a `mappend` b

tell' :: Monad m => [r] -> AmbT r m ()
tell' t = AmbT $ (lift $ lift $ tellState t)

forEffects :: Monad m => ((t, [t]) -> r) -> (t1 -> AmbT t m t) -> AmbT t m t1 -> m r
forEffects f g e = f `liftM` runAmbTI (do ambCC $ \k -> do
                                            AmbT $ put (k undefined)
                                            v <- e
                                            g v)
                                      (return undefined)

-- Run nondeterministic computations

oneValueT :: Monad m => AmbT b m b -> m b
oneValueT c = runAmbT c >>= return . fst

oneValue :: Amb a a -> a
oneValue = runIdentity . oneValueT

allValuesT :: Monad m => AmbT t m t -> m [t]
allValuesT = forEffects snd (\a -> tell' [a] >> fail')

allValues :: Amb t t -> [t]
allValues = runIdentity . allValuesT

isPossibleT :: Monad m => AmbT Bool m Bool -> m Bool
isPossibleT = forEffects (([True] ==) . snd) (\a -> when (a == False) fail' >> tell' [True] >> return undefined)

isPossible :: Amb Bool Bool -> Bool
isPossible = runIdentity . isPossibleT

isNecessaryT :: Monad m => AmbT Bool m Bool -> m Bool
isNecessaryT = forEffects (([] ==) . snd) (\a -> when (a == True) fail' >> tell' [True] >> return undefined)

isNecessary :: Amb Bool Bool -> Bool
isNecessary = runIdentity . isNecessaryT

-- Generate nondeterministic computations

either' :: Monad m => AmbT r m b -> AmbT r m b -> AmbT r m b
either' a b = do r <- aBoolean
                 if r then a else b

fail' :: Monad m => AmbT r m b
fail' = AmbT get >>= (\a -> a >> return undefined)

aBoolean :: Monad m => AmbT r m Bool
aBoolean = ambCC $ \k -> do
             old <- AmbT get
             AmbT $ put (AmbT (put old) >> (k False) >> undefined)
             return True

aMemberOf :: Monad m => [b] -> AmbT r m b
aMemberOf [] = fail'
aMemberOf (x:xs) =  return x `either'` aMemberOf xs

aSubsetOf :: Monad m => [AmbT r m a] -> AmbT r m [a]
aSubsetOf [] = return []
aSubsetOf (x:xs) = aSubsetOf xs `either'` liftM2 (:) x (aSubsetOf xs)

anIntegerBetween :: (Monad m, Num b, Ord b) => b -> b -> AmbT r m b
anIntegerBetween i j | i > j = fail'
                     | otherwise = either' (return i) (anIntegerBetween (i + 1) j) 

aSplitOf :: Monad m => [a] -> AmbT r m ([a],[a])
aSplitOf l = loop [] l
    where loop x [] = return (x,[])
          loop x y@(y0:ys)  = either' (return (x,y)) (loop (x ++ [y0]) ys)

aPermutationOf :: Monad m => [a] -> AmbT r m [a]
aPermutationOf [] = return []
aPermutationOf (l0:ls) = do (s1,s2) <- (aPermutationOf ls >>= aSplitOf)
                            return $ s1 ++ (l0:s2)

aPartitionOf :: (Eq t, Monad m) => [t] -> AmbT r m [[t]]
aPartitionOf [] = return []
aPartitionOf (x:xs) = do y <- aPartitionOf xs
                         either' (return ([x]:y))
                                 (do z <- aMemberOf y
                                     return ((x:z) : filter (z /=) y))

aPartitionOfSize :: (Eq a, Monad m) => Int -> [a] -> AmbT r m [[a]]
aPartitionOfSize 0 _ = error "Can't create a partition of size 0"
aPartitionOfSize k l | length l < k = fail'
                     | otherwise = loop l
    where loop x@(x0:xs) | length x == k = return $ map (:[]) x
                         | otherwise = do y <- loop xs
                                          z <- aMemberOf y
                                          return ((x0:z):filter (z /=) y)
          loop [] = fail'

amb :: Monad m => [b] -> AmbT r m b
amb = aMemberOf
