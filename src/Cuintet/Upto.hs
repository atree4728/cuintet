module Cuintet.Upto (Upto (..), empty, toMaybes, head) where

import Clash.Prelude hiding (empty, head, toList)
import Clash.Prelude qualified as C
import Cuintet.Util (orNothing)

data Upto n a = Upto
  { len :: Index (n + 1)
  , elems :: Vec n a
  }
  deriving (Generic, NFDataX)

empty :: (KnownNat n, NFDataX a) => Upto n a
empty = Upto {len = 0, elems = deepErrorX "Upto.empty"}

toMaybes :: (KnownNat n) => Upto n a -> Vec n (Maybe a)
toMaybes Upto {..} = imap (\i e -> orNothing (numConvert i < len) e) elems

head :: (KnownNat n) => Upto (n + 1) a -> Maybe a
head Upto {..} = orNothing (len > 0) (C.head elems)
