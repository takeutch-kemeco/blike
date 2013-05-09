module Test016a where

import Blike

foreign export ccall
  hs_bl_main :: IO ()

hs_bl_main :: IO ()
hs_bl_main = do
  bl_fillOval 1 1 1 1
  bl_fillOval (640 - 1) (400 - 1) 1 1
  bl_setCol 0x0000ff
  bl_drawStr 192 136 8 8 (show 1234)
  bl_wait (-1)
