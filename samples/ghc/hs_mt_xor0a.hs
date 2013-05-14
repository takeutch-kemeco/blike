module Mt_xor0a where

import Blike
import Data.Bits

foreign export ccall
  hs_bl_main :: IO ()

hs_bl_main :: IO ()
hs_bl_main = do
  bl_openWin 160 48
  bl_setPix (49 + 30) 0 0xc6c6c6

  mapM_
    (\y -> mapM_
           (\x -> setPix x y)
           [(1 + 30)..(99 + 30 - 1)]
    ) [0..46]

  bl_setMode _BL_PXOR
  bl_setCol 0xc6c6c6
  bl_fillRect 160 48 0 0
  bl_setMode _BL_PSET
  mainLoop 0

  where
    setPix x y = do
      cl <- bl_getPix (x - 1) y
      cr <- bl_getPix (x + 1) y
      bl_setPix x (y + 1) (xor cl cr)

    c = [0x000000, 0xbf0000, 0x00bf00, 0xbfbf00,
         0x0000bf, 0xbf00bf, 0x00bfbf, 0xffffff]

    mainLoop :: Int -> IO ()
    mainLoop i = do
      mapM_
        (\y -> mapM_
               (\x -> bl_getPix x y >>=
                      (\col -> case col of
                          0xc6c6c6 -> return ()
                          otherwise -> bl_setPix x y (c !! i)
                      )
               ) [0..159]
        ) [0..47]
      bl_wait 500
      mainLoop ((i + 1) .&. 7)
