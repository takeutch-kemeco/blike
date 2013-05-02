module Test002a where

import Blike

foreign export ccall
  hs_bl_main :: IO ()

hs_bl_main :: IO ()
hs_bl_main =
  bl_openWin 256 256 >>

  mapM_
    (\y -> mapM_
           (\x -> bl_rgb x y 0 >>= (\c -> bl_setPix x y c))
           [0..255])
    [0..255] >>

  bl_wait (-1)
