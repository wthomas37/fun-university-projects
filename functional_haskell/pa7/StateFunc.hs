module StateFunc where

newtype StateFunc s a =
  StateFunc { runStateFunc :: s -> (a,s) }

instance Functor (StateFunc s) where
  fmap :: (a -> b) -> StateFunc s a -> StateFunc s b
  fmap g sa = StateFunc $ \s0 ->
    let
      (a,s1) = runStateFunc sa s0
    in
      (g a, s1)

instance Applicative (StateFunc s) where
  pure :: a -> StateFunc s a
  pure a = StateFunc $ \s -> (a, s)

  (<*>) :: StateFunc s (a -> b) -> StateFunc s a -> StateFunc s b
  sab <*> sa = StateFunc $ \s0 ->
    let
      (f, s1) = runStateFunc sab s0
      (a, s2) = runStateFunc sa s1
    in
      (f a, s2)

instance Monad (StateFunc s) where
  (>>=) :: StateFunc s a -> (a -> StateFunc s b) -> StateFunc s b
  sa >>= f = StateFunc $ \s0 ->
    let
      (a, s1) = runStateFunc sa s0
      (b, s2) = runStateFunc (f a) s1
    in
      (b, s2)

get           :: StateFunc s s                  -- get state out
put           :: s -> StateFunc s ()            -- set "current" state
modify        :: (s -> s) -> StateFunc s ()     -- modify the state
evalStateFunc :: StateFunc s a -> s -> a        -- run and return final value
execStateFunc :: StateFunc s a -> s -> s        -- run and return final state

get                = StateFunc $ \s -> (s, s)
put s'             = StateFunc $ \s -> ((), s')
modify f           = StateFunc $ \s -> ((), f s)
evalStateFunc sa s = fst $ runStateFunc sa s
execStateFunc sa s = snd $ runStateFunc sa s
