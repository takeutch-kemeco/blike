module Msgk01a where

import Blike

foreign export ccall
  hs_bl_main :: IO ()

hs_bl_main :: IO ()
hs_bl_main = do
  bl_openWin (width * 8) 16
  bl_setCol 0xffff00
  mainLoop msg
  
  where
    width = 60
    msg = take width ("OSASK" ++ (repeat ' '))

    mainLoop :: String ->  IO ()
    mainLoop s = do
      bl_locate 0 0
      bl_puts s
      bl_wait 100
      mainLoop $ (tail s) ++ [head s]
