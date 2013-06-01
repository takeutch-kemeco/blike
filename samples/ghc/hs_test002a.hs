module Test002a where

import Blike

foreign export ccall
  hs_bl_main :: IO ()

hs_bl_main :: IO ()
hs_bl_main = bl_openWin 256 256 >> fillRect >> bl_wait (-1)
  where
    setPix y x = bl_rgb x y 0 >>= (bl_setPix x y)
    drawLine y = mapM_ (setPix y) [0..255]
    fillRect = mapM_ drawLine [0..255]
    