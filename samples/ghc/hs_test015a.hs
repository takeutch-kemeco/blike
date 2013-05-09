module Test015a where

import Blike
import Data.Bits
import Data.Char

foreign export ccall
  hs_bl_main :: IO ()

data PenContext = PenContext {x, y, c, f, k :: Int}

drawA :: PenContext -> IO PenContext
drawA a@(PenContext x y c f k) | f == 0 = return a
                               | otherwise = do
  bl_setMode _BL_PXOR
  bl_setCol 0xffffff
  bl_drawRect 14 14 (x * 16 + 1) (y * 16 + 1)
  bl_flshWin 16 16 (x * 16) (y * 16)
  return a

getKey :: PenContext -> IO PenContext
getKey a@(PenContext x y c f _) = do
  bl_waitNF 10
  k <- bl_inkey1
  case k of
    0 -> getKey a
    otherwise -> return (PenContext x y c f k)

drawB :: PenContext -> IO PenContext
drawB a@(PenContext x y c f k) | f == 0 = return a
                               | otherwise = do
  bl_drawRect 14 14 (x * 16 + 1) (y * 16 + 1)
  bl_setMode _BL_PSET
  bl_flshWin 16 16 (x * 16) (y * 16)
  return a

drawC :: PenContext -> IO PenContext
drawC a@(PenContext x y c f k) | c < 0 = return a
                               | otherwise = do
  bl_setCol c
  bl_fillRect 16 16 (x * 16) (y * 16)
  bl_flshWin 16 16 (x * 16) (y * 16)
  return a

controlA :: PenContext -> IO PenContext
controlA (PenContext x y _ f k) = do
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
  return (PenContext x' y' c' f' k)

  where
    keyLeft k x | (k == _KEY_LEFT) && (x > 0) = return (x - 1)
                | otherwise = return x

    keyRight k x | (k == _KEY_RIGHT) && (x < 15) = return (x + 1)
                 | otherwise = return x

    keyUp k y | (k == _KEY_UP) && (y > 0) = return (y - 1)
              | otherwise = return y

    keyDown k y | (k == _KEY_DOWN) && (y < 15) = return (y + 1)
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

mainLoop :: PenContext -> IO ()
mainLoop a =
  drawA a >>=
  getKey >>=
  drawB >>=
  controlA >>=
  drawC >>=
  mainLoop >>
  return ()

hs_bl_main :: IO ()
hs_bl_main = do
  bl_setBCol 0xffffff
  bl_openWin 256 256
  mainLoop (PenContext 0 0 0 1 0)

