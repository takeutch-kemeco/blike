module Test010a where

import Blike
import Data.Bits

foreign export ccall
  hs_bl_main :: IO ()

mainLoop =
  bl_inkey 0 >>=
  (\x -> case x of {0 -> bl_wait 100 >> return(); otherwise -> return()}) >>
  bl_inkey (_BL_WAITKEY .|. _BL_GETKEY .|. _BL_DELFFF) >>=
  bl_puts . (\x -> (show x) ++ " ")  >>
  mainLoop

hs_bl_main :: IO ()
hs_bl_main = mainLoop >> bl_wait 1
