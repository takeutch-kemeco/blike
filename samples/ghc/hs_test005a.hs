module Test005a where

import Blike

foreign export ccall
  hs_bl_main :: IO ()

fillRainbow = mapM_ lineRainbow [0..255]
  where
    lineRainbow y = mapM_ (pixRainbow y) [0..255]
    pixRainbow x y = bl_rgb x y 0 >>= (bl_setPix x y)

putColorStr s c x y = bl_locate x y >> bl_iCol c >>= bl_setCol >> bl_puts1 s

hs_bl_main :: IO ()
hs_bl_main = bl_openWin 256 256 >> fillRainbow >> putColorStr "hello" 7 13 8 >> bl_wait (-1)
