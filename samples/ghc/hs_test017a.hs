module Test017a where

import Blike
import Data.Bits
import Data.Char

foreign export ccall
  hs_bl_main :: IO ()

data GameContext = GameContext {sc, x, bx, by, t, osc, ox :: Int}

titleLoop :: IO ()
titleLoop = do
  bl_setCol 0x0000ff
  bl_drawStr 32 176 2 2 "Hit [Enter] to start"
  key <- bl_inkey $ _BL_WAITKEY .|. _BL_GETKEY
  if key == _KEY_ENTER
    then bl_cls >> bl_flshWin 384 384 0 0 >> return ()
    else titleLoop

drawScore :: GameContext -> IO GameContext
drawScore gc@(GameContext sc x bx by t osc ox)
  | sc == osc = return gc
  | otherwise = do
    bl_setCol 0xffffff
    bl_fillRect 160 32 16 352
    bl_setCol 0x000000
    bl_drawStr 16 352 2 2 ("SCORE = " ++ (show sc))
    let osc' = sc
    return (GameContext sc x bx by t osc' ox)

drawStack :: GameContext -> IO GameContext
drawStack gc@(GameContext sc x bx by t osc ox)
  | ox == x = return gc
  | otherwise = do
    bl_setCol 0xffffff
    bl_fillRect 20 8 (ox - 2) 336
    if sc > 0
      then bl_fillRect 16 ((sc + 1) * 16) ox (320 - sc * 16)
      else return ()
    bl_setCol 0x0000ff
    bl_fillRect 20 8 (x - 2) 336
    drawSC 0 sc
    bl_flshWin 20 8 (ox - 2) 336
    bl_flshWin 20 8 (x - 2) 336
    if sc > 0
      then bl_flshWin 16 (sc * 16) ox (336 - sc * 16) >>
           bl_flshWin 16 (sc * 16) x (336 - sc * 16) >>
           return ()
      else return ()
    let ox' = x
    return (GameContext sc x bx by t osc ox')
    where
      drawSC k sc
        | k < sc = bl_setCol 0xb8860b >>
                   bl_fillOval 16 16 x (320 - k * 16) >>
                   drawSC (k + 1) sc
        | otherwise = return()

drawDownBoal :: GameContext -> IO GameContext
drawDownBoal gc@(GameContext sc x bx by t osc ox)
  | by == (-1) = do
    bx' <- bl_rnd 24 >>= return . (* 16) 
    let by' = 0
    bl_setCol 0xb8860b
    bl_fillOval 16 16 bx' by'
    bl_flshWin 16 16 bx' by'
    return (GameContext sc x bx' by' t osc ox)
  | otherwise = do
    bl_setCol 0xffffff
    bl_fillRect 16 16 bx (by - 16)
    bl_setCol 0xb8860b
    bl_fillOval 16 16 bx by
    bl_flshWin 16 32 bx (by - 16)
    return gc

isGameOver :: GameContext -> Bool
isGameOver (GameContext _ _ _ by _ _ _)
  | by >= 336 = True
  | otherwise = False

controlA :: GameContext -> IO GameContext
controlA gc@(GameContext sc x bx by t osc ox) = do
  key <- bl_inkey1
  keyLeft key gc >>= keyRight key
  where
    keyLeft key gc@(GameContext sc x bx by t osc ox) = do
      if ((key == _KEY_LEFT) || (key == (ord '4'))) &&
         ((x > 0) && (not ((by > (320 - sc * 16)) && (bx == (x - 16)))))
        then return (GameContext sc (x - 16) bx by t osc ox)
        else return gc
    keyRight key gc@(GameContext sc x bx by t osc ox) = do
      if ((key == _KEY_RIGHT) || (key == (ord '6'))) &&
         ((x < 368) && (not ((by > (320 - sc * 16)) && (bx == (x + 16)))))
        then return (GameContext sc (x + 16) bx by t osc ox)
        else return gc

nextBoal :: GameContext -> IO GameContext
nextBoal gc@(GameContext sc x bx by t osc ox) =
  case (bx == x) && (by == (320 - sc * 16)) of
    True -> return (GameContext (sc + 1) x bx (-1) t osc ox)
    False -> return gc

moveBoal :: GameContext -> IO GameContext
moveBoal gc@(GameContext sc x bx by t osc ox) = case t' >= 4 of
  True -> return (GameContext sc x bx (by + 16) 0 osc ox)
  False -> return (GameContext sc x bx by t' osc ox)
  where
    t' = t + 1

updateBoal :: GameContext -> IO GameContext
updateBoal gc = nextBoal gc >>= moveBoal
    
gameLoop :: GameContext -> IO ()
gameLoop gc@(GameContext sc x bx by t osc ox) = do
  gc' <- (drawScore gc >>= drawStack >>= drawDownBoal)
  case isGameOver gc' of
    True -> return ()
    False -> bl_waitNF 50 >> controlA gc' >>= updateBoal >>= gameLoop

mainLoop :: IO ()
mainLoop = do
  titleLoop
  gameLoop (GameContext 0 176 0 (-1) 0 9 0)
  mainLoop
  return()

hs_bl_main :: IO ()
hs_bl_main = do
  bl_setBCol 0xffffff
  bl_openWin 384 384
  mainLoop
