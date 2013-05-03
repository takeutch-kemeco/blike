module Test005a where

import Blike

foreign export ccall
  hs_bl_main :: IO ()

fillRainbow :: IO ()
fillRainbow = do
  mapM_
    (\y -> mapM_
           (\x -> bl_rgb x y 0 >>=
                  (\c -> bl_setPix x y c))
           [0..255])
    [0..255]

hs_bl_main :: IO ()
hs_bl_main = do
  bl_openWin 256 256
  
  fillRainbow

  bl_locate 13 8
  bl_iCol 7 >>= bl_setCol
  bl_puts1 "hello"
  bl_wait (-1)
