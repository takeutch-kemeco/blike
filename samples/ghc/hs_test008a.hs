module Test008a where

import Blike
import Data.Char

foreign export ccall
  hs_bl_main :: IO ()

drawString x y s = bl_locate x y >> bl_puts1 s

mainLoop b c bb cc d = do
  drawString b c " "
  drawString d 23 "     "

  key <- bl_inkey1
  d' <- update_d key

  drawString b' c' "O"
  drawString d' 23 "#####"

  bl_wait 100

  case c' >= 23 of {True -> return (); False -> mainLoop b' c' bb' cc'' d'}

  where
    bb' = case b of {0 -> 1; 78 -> (-1); otherwise -> bb}
    cc' = case (d - 1 <= b) && (b <= d + 5) of {True -> (-1); False -> cc}
    cc'' = case c of {0 -> 1; 22 -> cc'; otherwise -> cc}

    b' = b + bb'
    c' = c + cc''
    update_d key = return d3 
      where
        d0 = if (key == _KEY_RIGHT) && (d < 73) then d + 2 else d
        d1 = if (key == _KEY_LEFT) && (d0 > 1)  then d0 - 2 else d0
        d2 = if (key == ord '6') && (d1 < 73) then d1 + 2 else d1
        d3 = if (key == ord '4') && (d2 > 1)  then d2 - 2 else d2

hs_bl_main :: IO ()
hs_bl_main = do
  bl_cls
  mainLoop 39 11 1 (-1) 37
  bl_wait (-1)
