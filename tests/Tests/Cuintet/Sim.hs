-- | テスト用シミュレーションヘルパ。
module Tests.Cuintet.Sim (memImage, runProgram, finalRegs) where

import Clash.Prelude
import qualified Prelude as P

import Clash.Sized.Vector (unsafeFromList)
import Cuintet (system)
import Cuintet.Core (InstLog (..))
import Cuintet.Eei (Inst, XLen)
import Data.Maybe (catMaybes)

-- | RISC-V の canonical NOP (@addi x0, x0, 0@)。
nop :: Inst
nop = 0x00000013

{- | プログラムを 256 語のメモリイメージに詰める。
余りを NOP で埋めるので、プログラム末尾を越えてフェッチしても未定義命令にならない。
-}
memImage :: [Inst] -> Vec 256 Inst
memImage prog = unsafeFromList (P.take 256 (prog P.++ P.repeat nop))

{- | プログラムを走らせ、最初の @n@ 命令分の commit ログを返す。
サイクル予算は余裕を持たせてあり、@n@ 個揃わずに足りない場合は増やす。
-}
runProgram :: Int -> [Inst] -> [InstLog]
runProgram n prog =
  P.take n (catMaybes (sampleN @System (16 + 8 * n) (system (blockRam (memImage prog)))))

-- | commit ログの書き戻しを畳み込んでレジスタファイルを再構成する。
finalRegs :: Int -> [Inst] -> Vec 32 (BitVector XLen)
finalRegs n prog = P.foldl apply (replicate d32 0) (runProgram n prog)
 where
  apply regs l = maybe regs (\(a, d) -> replace a d regs) l.wbReq
