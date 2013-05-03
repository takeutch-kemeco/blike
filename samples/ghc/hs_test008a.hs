module Test008a where

import Blike

foreign export ccall
  hs_bl_main :: IO ()

drawString x y s = bl_locate x y >> bl_puts1 s

mainLoop b c bb cc d = do
  drawString b c " "
  drawString d 23 "     "

  bb <- return $ case b of
    0 -> 1
    78 -> (-1)
    otherwise -> bb

  cc <- return $ case c of
    0 -> 1
    22 -> if (d - 1 <= b) && (b <= d + 5) then (-1) else cc
    otherwise -> cc

  b <- return (b + bb)
  c <- return (c + cc)

  a <- bl_inkey1
  d <- return $ if (a == 333) && (d < 73) then d + 2 else d
  d <- return $ if (a == 331) && (d > 1)  then d - 2 else d
--  d <- return $ if (a == '6') && (d < 73) then d + 2 else d
--  d <- return $ if (a == '4') && (d > 1)  then d - 2 else d

  drawString b c "O"
  drawString d 23 "#####"

  bl_wait 100

  if c >= 23 then return () else mainLoop b c bb cc d

hs_bl_main :: IO ()
hs_bl_main = do
  bl_cls
  mainLoop 39 11 1 (-1) 37
  bl_wait (-1)
