import Clash.Main (defaultMain)
import Prelude
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= defaultMain . ("--interactive" :)
