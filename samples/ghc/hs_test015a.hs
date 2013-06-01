module Test015a where

import Blike
import Data.Bits
import Data.Char

foreign export ccall
  hs_bl_main :: IO ()

data PenContext = PenContext {pc_x, pc_y, pc_c, pc_f, pc_k :: Int}

drawA :: PenContext -> IO PenContext
drawA pc@(PenContext x y _ f _)
  | f == 0 = return pc
  | otherwise = bl_setMode _BL_PXOR >>
                bl_setCol 0xffffff >>
                bl_drawRect 14 14 (x * 16 + 1) (y * 16 + 1) >>
                bl_flshWin 16 16 (x * 16) (y * 16) >>
                return pc

getKey :: PenContext -> IO PenContext
getKey pc = bl_waitNF 10 >> bl_inkey1 >>= loop
  where
    loop key | key == 0 = getKey pc 
             | otherwise = return (pc {pc_k = key})

drawB :: PenContext -> IO PenContext
drawB pc@(PenContext x y _ f _)
  | f == 0 = return pc
  | otherwise = bl_drawRect 14 14 (px + 1) (py + 1) >>
                bl_setMode _BL_PSET >>
                bl_flshWin 16 16 px py >>
                return pc
    where
      px = x * 16
      py = y * 16

drawC :: PenContext -> IO PenContext
drawC pc@(PenContext x y c _ _)
  | c < 0 = return pc
  | otherwise = bl_setCol c >>
                bl_fillRect 16 16 px py >>
                bl_flshWin 16 16 px py >>
                return pc
    where
      px = x * 16
      py = y * 16

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
mainLoop a = drawA a >>= getKey >>= drawB >>= controlA >>= drawC >>= mainLoop

hs_bl_main :: IO ()
hs_bl_main = bl_setBCol 0xffffff >> bl_openWin 256 256 >> mainLoop pc
  where
    pc = PenContext 0 0 0 1 0
