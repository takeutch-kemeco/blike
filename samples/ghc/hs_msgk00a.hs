module Msgk00a where

import Blike

foreign export ccall
  hs_bl_main :: IO ()

hs_bl_main :: IO ()
hs_bl_main = do
  bl_openWin (width * 8) 16
  bl_setCol 0xffff00
  mainLoop
  return ()
  
  where
    width = 60
    msg = "OSASK"

    mainLoop :: IO ()
    mainLoop = mapM_
               (\i -> mapM_
                      (\j -> clearChar j >>= putChar i >> bl_wait 100)
                      (reverse [(i + 1)..width]) >>
                      bl_wait 1000
               ) [0..((length msg) - 1)] >>

               bl_wait 5000 >> bl_cls >> mainLoop

    clearChar j | (j < width) = bl_locate j 0 >> bl_putc(' ') >> return j'
                |otherwise = return j'
      where j' = j - 1

    putChar i j = bl_locate j 0 >>
                  bl_putc (msg !! i)
