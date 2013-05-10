module Test018a where

import Blike
import Data.Bits
import Data.Char

foreign export ccall
  hs_bl_main :: IO ()

data GameContext = GameContext {sc, x, y, vx, vy, ox, oy, x16, y16, t, lv :: Int}

titleLoop :: IO ()
titleLoop = do
  bl_setCol 0x00009f
  bl_fillRect 320 32 160 176
  bl_setCol 0xffffff
  bl_drawStr 160 176 2 2 "Hit [Enter] to start"
  key <- bl_inkey (_BL_WAITKEY .|. _BL_GETKEY)
  if key == _KEY_ENTER
    then return ()
    else titleLoop

drawAsteroid :: GameContext -> IO ()
drawAsteroid (GameContext sc x y vx vy ox oy x16 y16 t lv) = mapM_ draw [0..19]
  where
    draw _ = do
      bl_rnd 6 >>= (bl_iCol . (+ 1)) >>= bl_setCol
      px <- bl_rnd (512 - lv * 8) >>= return . (+ 64)
      py <- bl_rnd (368 - lv * 8) >>= return . (+ 32)
      bl_fillOval (lv * 8) (lv * 8) px py
      return ()

stageInit :: GameContext -> IO GameContext
stageInit gc@(GameContext sc _ _ _ _ ox oy _ _ _ lv) = do
  bl_cls
  bl_setMode _BL_PSET
  bl_setCol 0xffffff
  bl_rnd 176 >>= ((bl_fillRect 32 32 32) . (+ 160))

  drawAsteroid gc

  let x = 630
      y = 40
      vx = 0
      vy = 0
      t = lv * 1000 + 990
      x16 = x * 16
      y16 = y * 16

  bl_flshWin 999 999 0 0
  return (GameContext sc x y vx vy ox oy x16 y16 t lv)

drawScore :: GameContext -> IO ()
drawScore (GameContext sc x y vx vy ox oy x16 y16 t lv)
  | (mod t 10) == 0 = do
    bl_setMode _BL_PSET
    bl_setCol 0x000000
    bl_fillRect 640 32 0 0
    bl_setCol 0xfefefe
    let str = "SCORE=" ++ (show sc) ++ "  LV=" ++ (show lv) ++ "  TIME=" ++ (show (div t 10))
    bl_drawStr 0 0 2 2 str
    bl_flshWin 640 32 0 0
    return ()
  | otherwise = return ()

drawStarship :: GameContext -> IO()
drawStarship (GameContext sc x y vx vy ox oy x16 y16 t lv) = do
  bl_setMode _BL_PXOR
  bl_setCol 0xffffff
  bl_fillRect 3 3 (x - 2) (y - 4)
  bl_fillRect 5 5 (x - 3) (y - 1)
  bl_flshWin 5 8 (x - 3) (y - 4)
  bl_flshWin 5 8 (ox - 3) (oy - 4)
  return ()


controlA :: GameContext -> IO GameContext
controlA (GameContext sc x y vx vy ox oy x16 y16 t lv) = do
  bl_waitNF 100
  key <- bl_inkey1
  bl_fillRect 3 3 (x - 2) (y - 4)
  bl_fillRect 5 5 (x - 3) (y - 1)
  f key
  where
    f key = return (GameContext sc x' y' vx'' vy'' ox' oy' x16' y16' t' lv)
      where
        ox' = x
        oy' = y
        vy' | ((key == (ord '2')) || (key == _KEY_DOWN)) && (vy > (-128)) = vy - 4
            | otherwise = vy
        vy'' = vy' + 1 
        vx' | ((key == (ord '4')) || (key == _KEY_LEFT)) && (vx < 128) = vx + 4
            | otherwise = vx
        vx'' | ((key == (ord '6')) || (key == _KEY_RIGHT)) && (vx > (-128)) = vx' - 4
             | otherwise = vx'
        x16' = x16 + vx''
        y16' = y16 + vy''
        x' = div x16' 16
        y' = div y16' 16
        t' | t > 0 = t - 1
           | otherwise = t

gameOver :: GameContext -> IO ()
gameOver gc@(GameContext sc x y vx vy ox oy x16 y16 t lv) = do
  bl_setCol 0xff0000
  bl_setMode _BL_PSET
  mapM_ draw [3,6..24]
  return ()
  where
    draw k = do
      bl_waitNF 200
      bl_drawRect (k * 2) (k * 2) (x - k) (y - k)
      bl_flshWin (k * 2) (k * 2) (x - k) (y - k)

nextStage :: GameContext -> IO GameContext
nextStage (GameContext sc x y vx vy ox oy x16 y16 t lv) = do
  let sc' = sc + (div t 10)
      lv' = case lv < 9 of {True -> lv + 1; False -> lv}
  bl_waitNF 2000
  return (GameContext sc' x y vx vy ox oy x16 y16 t lv')

stageOver :: Int -> GameContext -> IO ()
stageOver k gc | k /= 0xffffff = gameOver gc >> mainLoop
               | otherwise = nextStage gc >>= gameMain

gameLoop :: GameContext -> IO GameContext
gameLoop gc@(GameContext sc x y vx vy ox oy x16 y16 t lv) = do
  k <- bl_getPix x y
  drawScore gc
  drawStarship gc
  if k /= 0x000000
    then (stageOver k) gc >> return gc
    else controlA gc >>= gameLoop

gameMain :: GameContext -> IO ()
gameMain gc = do
  stageInit gc >>= gameLoop
  mainLoop
  return ()

mainLoop :: IO ()
mainLoop = do
  titleLoop
  bl_srand 1
  let gc = (GameContext 0 0 0 0 0 0 0 0 0 0 1)
  gameMain gc >> mainLoop
  return ()

hs_bl_main :: IO ()
hs_bl_main = do
  mainLoop
  return ()
