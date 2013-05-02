module Test000a where

import Blike

foreign export ccall
  hs_bl_main :: IO ()

hs_bl_main :: IO ()
hs_bl_main = do
  bl_puts1 "hello world\n"
  bl_wait (-1)
