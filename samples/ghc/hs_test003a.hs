module Test003a where

import Blike

foreign export ccall
  hs_bl_main :: IO ()

hs_bl_main :: IO ()
hs_bl_main = bl_openWin 256 256 >> drawA >> bl_wait (-1)
  where
    drawA = bl_setCol 0xffff00 >> bl_setMode _BL_PXOR >> fillRect
    drawLine x = bl_drawLine 0 0 x 255 >> bl_drawLine x 0 255 255
    fillRect = mapM_ drawLine [0..255]
    