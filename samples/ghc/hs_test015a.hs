module Test015a where

import Blike
import Data.Bits
import Data.Char

foreign export ccall
  hs_bl_main :: IO ()

drawA (x, y, c, f, k) | f == 0 = return ()
                      | otherwise = do
  bl_setMode _BL_PXOR
  bl_setCol 0xffffff
  bl_drawRect 14 14 (x * 16 + 1) (y * 16 + 1)
  bl_flshWin 16 16 (x * 16) (y * 16)
  return ()

getKey :: IO Int
getKey = do
  bl_waitNF 10
  k <- bl_inkey1
  case k of
    0 -> getKey
    otherwise -> return k

drawB (x, y, c, f, k) | f == 0 = return ()
                      | otherwise = do
  bl_drawRect 14 14 (x * 16 + 1) (y * 16 + 1)
  bl_setMode _BL_PSET
  bl_flshWin 16 16 (x * 16) (y * 16)
  return ()

drawC (x, y, c, f, k) | c < 0 = return ()
                      | otherwise = do
  bl_setCol c
  bl_fillRect 16 16 (x * 16) (y * 16)
  bl_flshWin 16 16 (x * 16) (y * 16)
  return ()

controlA (x, y, _, f, k) = do
  x' <- (keyLeft k x >>= keyRight k)
  y' <- (keyUp k y >>= keyDown k)
  f' <- keySpace k f
  let c = -1
  c' <- (keyA k c >>=
         keyS k >>=
         keyD k >>=
         keyF k >>=
         keyG k >>=
         keyH k >>=
         keyJ k >>=
         keyK k)
  return (x', y', c', f', k)

  where
    keyLeft k x | k == _KEY_LEFT = return (x - 1)
                | otherwise = return x

    keyRight k x | k == _KEY_RIGHT = return (x + 1)
                 | otherwise = return x

    keyUp k y | k == _KEY_UP = return (y - 1)
              | otherwise = return y

    keyDown k y | k == _KEY_DOWN = return (y + 1)
                | otherwise = return y

    keySpace k f | k == ord ' ' = return (xor f 1)
                 | otherwise = return f

    keyA k c | k == ord 'a' = return 0x000000
             | otherwise = return c

    keyS k c | k == ord 's' = return 0x0000ff
             | otherwise = return c

    keyD k c | k == ord 'd' = return 0x00ff00
             | otherwise = return c

    keyF k c | k == ord 'f' = return 0x00ffff
             | otherwise = return c

    keyG k c | k == ord 'g' = return 0xff0000
             | otherwise = return c

    keyH k c | k == ord 'h' = return 0xff00ff
             | otherwise = return c

    keyJ k c | k == ord 'j' = return 0xffff00
             | otherwise = return c
                                            
    keyK k c | k == ord 'k' = return 0xffffff
             | otherwise = return c

mainLoop :: (Int, Int, Int, Int, Int) -> IO ()
mainLoop a@(x, y, c, f, k) = do
  drawA a
  k' <- getKey
  drawB a
  a' <- controlA (x, y, c, f, k')
  drawC a'
  mainLoop a'
  return ()

hs_bl_main :: IO ()
hs_bl_main = do
  bl_setBCol 0xffffff
  bl_openWin 256 256
  mainLoop (0, 0, 0, 1, 0)
  return ()
