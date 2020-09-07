{-# LANGUAGE LambdaCase #-}
module Scanner where

-- | Brent Yorgey's blog on Scanner gave me the necessary inspiration
--   to make my own library to scan and parse inputs.

-- | Thank you for your amazing post, Mr Yorgey!

import State
import Control.Monad (liftM2, replicateM)

type Scanner = State [String]

runScanner :: Scanner a -> String -> a
runScanner s = evalState s . words

str :: Scanner String
str = get >>= \case {s:ss -> put ss >> return s}

int :: Scanner Int
int = read <$> str

integer :: Scanner Integer
integer = read <$> str

double :: Scanner Double
double = read <$> str

numberOf :: Scanner a -> Scanner [a]
numberOf s = int >>= flip replicateM s

many :: Scanner a -> Scanner [a]
many s = get >>= \case { [] -> return [];
                         _  -> liftM2 (:) s (many s)
                       }

repeat :: Int -> Scanner a -> Scanner [a]
repeat n s = replicateM n s

two, three :: Scanner a -> Scanner [a]
[two, three] = map replicateM [2,3]
