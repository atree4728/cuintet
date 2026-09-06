module Cuintet.Upto (Upto (..), none, toMaybes, first) where

import Clash.Prelude hiding (toList)
import Clash.Prelude qualified as C
import Cuintet.Util (orNothing)
import Data.Maybe (catMaybes)

data Upto n a = Upto
  { len :: Index (n + 1)
  , elems :: Vec n a
  }
  deriving (Generic, NFDataX)

none :: (KnownNat n, NFDataX a) => Upto n a
none = Upto {len = 0, elems = deepErrorX "Upto.none"}

toMaybes :: (KnownNat n) => Upto n a -> Vec n (Maybe a)
toMaybes Upto {..} = imap (\i e -> orNothing (numConvert i < len) e) elems

first :: (KnownNat n) => Upto (n + 1) a -> Maybe a
first Upto {..} = orNothing (len > 0) (head elems)
