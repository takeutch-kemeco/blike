module Test010a where

import Blike
import Data.Bits

foreign export ccall
  hs_bl_main :: IO ()

mainLoop =
  bl_inkey 0 >>= waitKey >>
  bl_inkey (_BL_WAITKEY .|. _BL_GETKEY .|. _BL_DELFFF) >>=
  bl_puts . (\x -> (show x) ++ " ")  >>
  mainLoop
  where
    waitKey k | k == 0 = bl_wait 100
              | otherwise = return ()

hs_bl_main :: IO ()
hs_bl_main = mainLoop >> bl_wait 1
