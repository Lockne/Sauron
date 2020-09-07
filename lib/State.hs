module State where

import Control.Monad (ap)
import Control.Applicative (Applicative (..), liftA)
import Control.Monad.Fail

newtype State s a = State {runState :: s -> (s, a)}

instance Functor (State s) where
    fmap = liftA

instance Monad (State s) where
    return = pure
    m >>= k = State $ \s -> case runState m s of
       (s', x) -> runState (k x) s'

instance Applicative (State s) where
    pure x = State $ \s -> (s, x)
    (<*>) = ap

instance MonadFail (State s) where
    fail = error "Pattern doesn't match"

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> (s, ())

evalState :: State s a -> s -> a
evalState m x = snd $ runState m x
