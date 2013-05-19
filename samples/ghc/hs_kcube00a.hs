module KCube00a where

import Data.List
import Blike

foreign export ccall
  hs_bl_main :: IO ()

type Vector = [Float]
type Matrix = [Vector]

innerProduct a b = sum $ map (\(x, y) -> x * y) (zip a b)

crossProduct a (bx:by:bz:[]) = concat $ mulMatrix [a] m
  where
    m = [[0,bz,(-by)],[(-bz),0,bx],[by,(-bx),0]]

mulMatrix a b = map (\c -> map (\h -> innerProduct h c) a) $ transpose b

rotMatrix (x:y:z:[]) = mulMatrix (mulMatrix mz my) mx
  where
    mx = [[1,0,0],[0,c,(-s)],[0,s,c]] where s = sin x; c = cos x
    my = [[c,0,s],[0,1,0],[(-s),0,c]] where s = sin y; c = cos y
    mz = [[c,(-s),0],[s,c,0],[0,0,1]] where s = sin z; c = cos z

rotVector v r = concat $ mulMatrix (rotMatrix r) (transpose [v])

type Vertex = Vector
type Rotate = Vector

data Polygon = Polygon {poly_color :: Int, poly_center_z :: Float,
                        poly_vertex :: [Vertex]} deriving (Show)

data Object = Object {obj_center_z :: Int, obj_polygon :: [Polygon]} deriving (Show)

data Screen = Screen {scr_a, scr_b, scr_x0, scr_y0 :: Float,
                      x_vsize, x_bsize, y_vsize :: Int, fillbuf :: [[Int]]} deriving (Show)

cube = Object 0 polygon
  where
    square = [[0,4,6,2],[1,3,7,5],[0,2,3,1],[0,1,5,4],[4,5,7,6],[6,7,3,2]] :: [[Int]]
    color = [0xff0000, 0x00ff00, 0xffff00, 0x0000ff, 0xff00ff, 0x00ffff] :: [Int]
    vertex = [[a,a,a],[a,a,b],[a,b,a],[a,b,b],[b,a,a],[b,a,b],[b,b,a],[b,b,b]] :: [Vertex]
    a = 100
    b = (-100)
    polygon = map (\(s, c) -> Polygon c 0 (map (\i -> vertex !! i) s)) (zip square color)

width = 160
height = 160
scrn = Screen 150 400 (toFloat (div width 2)) (toFloat (div height 2))
       width width height [[]]
theta = [0,0,0] :: Rotate

toFloat i = fromRational $ toRational $ fromInteger $ toInteger i

addRotate (x:y:z:[]) = (x + rad) : (y + (1.5 * rad)) : (z + (2 * rad)) : []
  where
    rad = pi / 180

rotPolygon (Polygon c _ vs) r = Polygon c z' vs'
  where
    vs' = map (\v -> rotVector v r) vs
    z' = (sum $ map (\v -> v !! 2) vs') / (fromIntegral (length vs'))

rotObject (Object z ps) r = Object z $ map (\p -> rotPolygon p r) ps

mapPolygon2D p s = map (\v -> f v) (poly_vertex p)
  where
    f (x:y:z:[]) = map (\a -> floor a) $ ((scr_x0 s) + x * t) : ((scr_y0 s) + y * t) : []
      where
        t = (scr_a s) / ((scr_b s) + z)

type OrderingTable = [(Int,Polygon)] 

blankOT = []::[(Int,Polygon)]

pushPolygonOT :: OrderingTable -> Polygon -> OrderingTable
pushPolygonOT ot p = insertBy f ((floor (poly_center_z p)), p) ot
  where
    f (ia, a) (ib, b) | ia == ib = EQ
                      | ia > ib = LT -- 故意
                      | ia < ib = GT -- 降順にソートする為に逆にしてある

pushObjectOT :: OrderingTable -> Object -> OrderingTable
pushObjectOT ot o = foldl (\ot' p -> pushPolygonOT ot' p) ot (obj_polygon o)

drawPolygon :: Polygon -> Screen -> IO ()
drawPolygon p s = do
  let vs = mapPolygon2D p s
      ls = zip vs ((tail vs) ++ [(last vs)])
  bl_setCol 0xffffff
  mapM_ (\((ax:ay:[]), (bx:by:[])) -> bl_drawLine ax ay bx by) ls
  return ()

drawOT :: OrderingTable -> Screen -> IO ()
drawOT o s = do
  mapM_ (\(_,p) -> drawPolygon p s) o
  return ()

hs_bl_main :: IO ()
hs_bl_main = do
  bl_openWin width height
  mainLoop theta
  return ()
  where
    mainLoop theta = do
      let theta' = addRotate theta
          cube' = rotObject cube theta'
          ot = pushObjectOT blankOT cube'
          
      bl_setCol 0x000000
      bl_fillRect width height 0 0
      drawOT ot scrn
      bl_wait 50
      
      mainLoop theta'
      return ()
