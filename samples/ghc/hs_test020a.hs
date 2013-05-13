module Test020a where

import Blike
import Data.Bits
import Data.Char

foreign export ccall
  hs_bl_main :: IO ()

hs_bl_main :: IO ()
hs_bl_main = do
  bl_openWin 640 384
  bl_slctWin 1
  bl_fillOval 1 1 1 1
  bl_fillOval (640 - 1) (384 - 1) 1 1
  bl_setCol 0x0000ff
  bl_drawStr 192 136 8 8 (show 1234)
  bl_copyRct0 640 384 1 0 0 0 0 0
  bl_slctWin 0
  bl_flshWin 640 384 0 0
  x <- return 320
  y <- return 192
  bl_setCol 0xffff00
  mainLoop x y
  return ()
  where
    mainLoop x y = do
      bl_fillOval 32 32 x y
      bl_flshWin 32 32 x y
      k <- bl_inkey (_BL_GETKEY .|. _BL_WAITKEYNF)
      bl_copyRct0 32 32 1 x y 0 x y
      bl_flshWin 32 32 x y
      x <- (if (k == _KEY_LEFT) && (x > 0) then return (x - 32) else return x)
      x <- (if (k == _KEY_RIGHT) && (x < (640 - 32)) then return (x + 32) else return x)
      y <- (if (k == _KEY_UP) && (y > 0) then return (y - 32) else return  y)
      y <- (if (k == _KEY_DOWN) && (y < (384 - 32)) then return (y + 32) else return y)
      mainLoop x y

