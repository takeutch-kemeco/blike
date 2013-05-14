module BBall00a where

import Blike

foreign export ccall
  hs_bl_main :: IO ()

data Point = Point {pointx, pointy :: Int}

t = [Point 196 100, Point 187  61, Point 164  29, Point 129   9, Point  90   5,
     Point  53  17, Point  23  44, Point   7  81, Point   7 119, Point  23 156,
     Point  53 183, Point  90 195, Point 129 191, Point 164 171, Point 187 139,
     Point 196 100]

c = [0x000000, 0xff0000, 0x00ff00, 0xffff00,
     0x0000ff, 0xff00ff, 0x00ffff, 0xffffff]

hs_bl_main :: IO ()
hs_bl_main = do
  bl_openWin 200 200
  bl_setMode _BL_POR

  mapM_ (\i ->do
            x0 <- return $ pointx (t !! i)
            y0 <- return $ pointy (t !! i)
            mapM_ (\j -> drawColorLine x0 y0 i j) [(i + 1)..15]
        ) [0..14]

  bl_wait (-1)

  where
    drawColorLine x0 y0 i j
      | (dis /= 0) = bl_setCol(c !! (8 - dis)) >> drawLine x0 y0 j >> return ()
      | otherwise = return ()
      where
        dis | (d >= 8) = 15 - d
            | otherwise = d
          where
            d = j - i
    drawLine x0 y0 j
      | (x0 <= (pointx (t !! j))) = bl_drawLine x0 y0 (pointx (t !! j)) (pointy (t !! j))
      | otherwise = bl_drawLine (pointx (t !! j)) (pointy (t !! j)) x0 y0
