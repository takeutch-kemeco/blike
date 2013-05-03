module Test003a where

import Blike

foreign export ccall
  hs_bl_main :: IO ()

hs_bl_main :: IO ()
hs_bl_main = do
  bl_openWin 256 256
  bl_setCol 0xffff00
  let bL_PXOR = 0x00000007
  bl_setMode bL_PXOR
  mapM_
    (\x -> bl_drawLine 0 0 x 255 >>
           bl_drawLine x 0 255 255)
    [0..255]
  bl_wait (-1)
