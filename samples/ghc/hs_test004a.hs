module Test004a where

import Blike

foreign export ccall
  hs_bl_main :: IO ()

setColor i = bl_setCol (c !! i)
  where
    c = [0x000000, 0xff0000, 0x00ff00, 0xffff00,
         0x0000ff, 0xff00ff, 0x00ffff, 0xffffff]

fillRect = mapM_ drawLine [0,2..320]

drawLine x = draw x >> bl_wait 30
  where
    draw x = bl_fillRect 1 200 x 0 >> bl_fillRect 1 200 (319 - x) 0

mainLoop i = setColor i >> fillRect >> mainLoop (mod (i + 1) 8)

hs_bl_main :: IO ()
hs_bl_main = bl_openWin 320 200 >> mainLoop 1
