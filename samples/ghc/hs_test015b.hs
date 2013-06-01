module Test015b where

import Blike
import Data.Bits
import Data.Char

foreign export ccall
  hs_bl_main :: IO ()

data Position = Position {pos_px, pos_py :: Int}

data RectContext = RectContext {rc_x, rc_y, rc_w, rc_h, rc_col :: Int}

data CursorContext = CursorContext {
  cc_pos :: Position,
  cc_bg :: RectContext,
  cc_boarder :: RectContext,
  cc_boarderShow :: Bool,
  cc_key :: Int}

drawRect :: RectContext -> (Int -> Int -> Int -> Int -> IO ()) -> Int -> IO ()
drawRect (RectContext x y w h col) draw pixelMode
  | col == (-1) = return ()
  | otherwise = bl_setMode pixelMode >> bl_setCol col >> draw w h x y >> bl_flshWin w h x y >> return ()

xorCursorBoarder :: CursorContext -> IO CursorContext
xorCursorBoarder cc@(CursorContext _ _ boarder boarderShow _)
  | boarderShow == True = drawRect boarder (bl_drawRect) _BL_PXOR >> return cc
  | otherwise = return cc

drawCursorBG :: CursorContext -> IO ()
drawCursorBG (CursorContext _ bg _ _ _) = drawRect bg (bl_fillRect) _BL_PSET

drawCursor :: CursorContext -> IO CursorContext
drawCursor cc = drawCursorBG cc >> xorCursorBoarder cc

getKey :: CursorContext -> IO CursorContext
getKey cc@(CursorContext a b c d _) = bl_waitNF 10 >> bl_inkey1 >>= loop
  where
    loop key | key == 0 = getKey cc
             | otherwise = return (cc {cc_key = key})

controlA :: CursorContext -> CursorContext
controlA (CursorContext
          (Position px py)
          (RectContext bgx bgy bgw bgh bgcol)
          (RectContext bx by bw bh bcol)
          boarderShow
          key)
  = (CursorContext
     (Position px' py')
     (RectContext (px' * 16) (py' * 16) bgw bgh bgcol')
     (RectContext (px' * 16 + 1) (py' * 16 + 1) bw bh bcol)
     boarderShow'
     key)
  where
    fkey :: Int -> (a -> Bool) -> (a -> a) -> (Int, a) -> (Int, a)
    fkey t fcomp fset (k,n) | (k == t) && (fcomp n) = (k, fset n)
                            | otherwise = (k, n)

    px' = snd $
          (fkey _KEY_RIGHT (< 15) (+ 1)) .
          (fkey _KEY_LEFT (> 0) (+ (-1))) $ (key, px)

    py' = snd $ (fkey _KEY_DOWN (< 15) (+ 1)) .
          (fkey _KEY_UP (> 0) (+ (-1))) $ (key, py)

    boarderShow' = snd $ fkey (ord ' ') (\x -> True) (not) (key, boarderShow) 

    bgcol' = snd $
             (fkey (ord 'k') (\x -> True) (\x -> 0xffffff)) .
             (fkey (ord 'j') (\x -> True) (\x -> 0xffff00)) .
             (fkey (ord 'h') (\x -> True) (\x -> 0xff00ff)) .
             (fkey (ord 'g') (\x -> True) (\x -> 0xff0000)) .
             (fkey (ord 'f') (\x -> True) (\x -> 0x00ffff)) .
             (fkey (ord 'd') (\x -> True) (\x -> 0x00ff00)) .
             (fkey (ord 's') (\x -> True) (\x -> 0x0000ff)) .
             (fkey (ord 'a') (\x -> True) (\x -> 0x000000)) $ (key, -1)

mainLoop :: CursorContext -> IO ()
mainLoop cc = drawCursor cc' >> getKey cc' >>= xorCursorBoarder >>= mainLoop
  where
    cc' = controlA cc

hs_bl_main :: IO ()
hs_bl_main = bl_setBCol 0xffffff >> bl_openWin 256 256 >> mainLoop cc
  where
    cc = CursorContext (Position 0 0) (RectContext 0 0 16 16 0) (RectContext 1 1 14 14 0xffffff) True 0
