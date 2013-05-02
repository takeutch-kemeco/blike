module Test001a where

import Blike

foreign export ccall
  hs_bl_main :: IO ()

hs_bl_main :: IO ()
hs_bl_main =
  mapM_
    (\a -> bl_color a 0 >> bl_puts1 ("color : " ++ (show a)))
    [0..15] >>

  bl_wait (-1)
