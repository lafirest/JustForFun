{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE GADTs #-}

module Reader where
import Freer

data Reader i o x where
  Get :: Reader i o i

type FReader i o a = Freer (Reader i o) a

run :: FReader i o x -> i -> x
run (Pure x) _ = x
run (Impure Get k) r = run (k r) r

ask = Impure Get return

test :: Freer (Reader Int Int) Int
test = do
  i <- ask
  return $ i + 12
