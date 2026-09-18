module Cuintet.Debug.Show (retireLines, hex) where

import Clash.Prelude
import Cuintet.Eei (BusWriteReq (..), MemAccess (..), StoreLanes (..), TrapCause (..))
import Cuintet.Pipeline (Retire (..))
import Text.Printf (printf)

retireLines :: Retire -> [String]
retireLines l =
  printf "%s : %s" (hex l.pc) (hex l.instBits)
    : foldMap (\(a, v) -> [printf "  reg[%2d] <= %s" (toInteger a) (hex v)]) l.rd
      <> foldMap (\r -> [memLine r]) l.mem
      <> foldMap (\c -> [trapLine c]) l.trap

memLine :: MemAccess -> String
memLine = \case
  LoadAccess addr -> printf "  mem[%s] load" (hex addr)
  StoreAccess BusWriteReq {addr, wdata = StoreLanes bytes} -> printf "  mem[%s] <= %s" (hex addr) (foldMap byte (reverse bytes))
  where
    byte = maybe "--" (printf "%02x" . toInteger)

trapLine :: TrapCause -> String
trapLine (TrapCause interrupt code) =
  printf "  trap: %s %d" (if interrupt then "interrupt" else "exception" :: String) (toInteger code)

hex :: (BitPack a) => a -> String
hex a
  | hasUndefined bv = "xxxxxxxx"
  | otherwise = printf "%08x" (toInteger bv)
  where
    bv = pack a
