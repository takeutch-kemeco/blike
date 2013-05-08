module Test011a where

import Blike

foreign export ccall
  hs_bl_main :: IO ()

hs_bl_main :: IO ()
hs_bl_main = bl_openWin 320 320 >> mainLoop >> bl_wait (-1)
  where
    mainLoop = bl_gets >>= bl_puts1 >> mainLoop
