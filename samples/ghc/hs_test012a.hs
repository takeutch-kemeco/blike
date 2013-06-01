module Test012a where

import Blike

foreign export ccall
  hs_bl_main :: IO ()

mainLoop j =
  bl_locate 0 0 >>
  bl_puts (toStr j 3) >> bl_flshWin 24 16 0 0 >> sleep 0 >>
  mainLoop (j + 1)

toStr j n  = reverse $ take n $ (reverse $ show j) ++ (repeat '0')

sleep i | i < 10 = bl_waitNF 100 >> sleep (i + 1)
        | otherwise = return ()

hs_bl_main :: IO ()
hs_bl_main = mainLoop 0
