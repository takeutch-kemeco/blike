module KCube00a where

import Data.List
import Blike

foreign export ccall
  hs_bl_main :: IO ()

type Vector = [Float]
type Matrix = [Vector]
type Vertex = Vector
type Normal = Vector
type Rotate = Vector

innerProduct :: Vector -> Vector -> Float
innerProduct a b = sum $ map (\(x, y) -> x * y) (zip a b)

crossProduct :: Vector -> Vector -> Vector
crossProduct a (bx:by:bz:[]) = concat $ mulMatrix [a] m
  where
    m = [[0,bz,(-by)],[(-bz),0,bx],[by,(-bx),0]]

mulMatrix :: Matrix -> Matrix -> Matrix
mulMatrix a b = map (\c -> map (\h -> innerProduct h c) a) $ transpose b

rotMatrix :: Rotate -> Matrix
rotMatrix (x:y:z:[]) = mulMatrix (mulMatrix mz my) mx
  where
    mx = [[1,0,0],[0,c,(-s)],[0,s,c]] where s = sin x; c = cos x
    my = [[c,0,s],[0,1,0],[(-s),0,c]] where s = sin y; c = cos y
    mz = [[c,(-s),0],[s,c,0],[0,0,1]] where s = sin z; c = cos z

rotVector :: Vector -> Rotate -> Vector
rotVector v r = concat $ mulMatrix (rotMatrix r) (transpose [v])

diffVector :: Vector -> Vector -> Vector
diffVector v1 v0 = map (\(a1, a0) -> a1 - a0) (zip v1 v0)

addVector :: Vector -> Vector -> Vector
addVector as bs = map (\(a, b) -> a + b) (zip as bs)

scaleVector :: Vector -> Float -> Vector
scaleVector v s = map (\a -> a * s) v

toNormal :: [Vertex] -> Normal
toNormal vs@(a:b:c:_) = crossProduct (diffVector b a) (diffVector c b)

data Polygon = Polygon {poly_color :: Int, poly_center_z :: Float,
                        poly_vertex :: [Vertex], poly_normal :: Vertex} deriving (Show)

data Object = Object {obj_center_z :: Int, obj_polygon :: [Polygon]} deriving (Show)

data Screen = Screen {scr_a, scr_b, scr_x0, scr_y0 :: Float} deriving (Show)

cube = Object 0 polygon
  where
    square = [[0,4,6,2],[1,3,7,5],[0,2,3,1],[0,1,5,4],[4,5,7,6],[6,7,3,2]] :: [[Int]]
    color = [0xff0000, 0x00ff00, 0xffff00, 0x0000ff, 0xff00ff, 0x00ffff] :: [Int]
    vertex = [[a,a,a],[a,a,b],[a,b,a],[a,b,b],[b,a,a],[b,a,b],[b,b,a],[b,b,b]] :: [Vertex]
    a = 100
    b = (-100)
    ps = map (\(s, c) -> Polygon c 0 (map (\i -> vertex !! i) s) []) (zip square color)
    polygon = map (\p -> p {poly_normal = (toNormal (poly_vertex p))}) ps

width = 160

height = 160

scrn = Screen 150 400 (toFloat (div width 2)) (toFloat (div height 2))

theta = [0,0,0] :: Rotate

toFloat :: (Integral a, Floating b) => a -> b
toFloat i = fromRational $ toRational $ fromInteger $ toInteger i

addRotate :: Rotate -> Rotate
addRotate (x:y:z:[]) = (x + rad) : (y + (1.5 * rad)) : (z + (2 * rad)) : []
  where
    rad = pi / 180

rotPolygon :: Polygon -> Rotate -> Polygon
rotPolygon (Polygon c _ vs n) r = Polygon c z' vs' n'
  where
    vs' = map (\v -> rotVector v r) vs
    z' = (sum $ map (\v -> v !! 2) vs') / (fromIntegral (length vs'))
    n' = rotVector n r

rotObject :: Object -> Rotate -> Object
rotObject (Object z ps) r = Object z $ map (\p -> rotPolygon p r) ps

mapPolygon2D :: Polygon -> Screen -> [Vector]
mapPolygon2D p s = map (\v -> f v) (poly_vertex p)
  where
    f (x:y:z:_) = ((scr_x0 s) + x * t) : ((scr_y0 s) + y * t) : []
      where
        t = (scr_a s) / ((scr_b s) + z)

type OrderingTable = [(Int,Polygon)]

blankOT = []::[(Int,Polygon)]

pushPolygonOT :: OrderingTable -> Polygon -> OrderingTable
pushPolygonOT ot p@(Polygon {poly_normal = (_:_:z:[])})
  | z <= 0 = ot
  | otherwise = insertBy f ((floor (poly_center_z p)), p) ot
  where
    f (ia, a) (ib, b) | ia == ib = EQ
                      | ia > ib = LT
                      | ia < ib = GT

pushObjectOT :: OrderingTable -> Object -> OrderingTable
pushObjectOT ot o = foldl (\ot' p -> pushPolygonOT ot' p) ot (obj_polygon o)

sortVertexY :: [Vertex] -> [Vertex]
sortVertexY vs = [min, mid, max]
  where
    min = foldl1 (\t@(_:ty:_) v@(_:vy:_) -> if vy < ty then v else t) vs
    max = foldl1 (\t@(_:ty:_) v@(_:vy:_) -> if vy > ty then v else t) vs
    mid = foldl1 (\t v -> if v /= min && v /= max then v else t) vs

splitHoriFlTr :: [Vertex] -> ([Vertex], [Vertex])
splitHoriFlTr vs = ([mid,p,max], [mid,p,min])
  where
    vs'@(min:mid:max:_) = sortVertexY vs
    l = diffVector max min
    p = addVector min $ scaleVector l $ ((diffVector mid min) !! 1) / (l !! 1)

drawFlTr :: [Vertex] -> Int -> IO ()
drawFlTr vs col = bl_setCol col >> drawHoriFlTr vsa >> drawHoriFlTr vsb
  where
    (vsa, vsb) = splitHoriFlTr vs
    
    drawHoriFlTr vs@(a:b:c:[]) = mapM_ drawLine ls 
      where
        la = diffVector c a
        lb = diffVector c b
        
        h = abs (la !! 1)
        ih = case h of {0 -> 0; otherwise -> 1 / h}
        da = scaleVector la ih
        db = scaleVector lb ih
        
        create top d n = reverse rvs
          where
            rvs = foldl (\ts@(t:_) a -> (addVector t d):ts) [top] (take (n - 1) $ repeat d)
        
        vsa = create a da (round h)
        vsb = create b db (round h)
        
        toIntVector vs = map (\v -> map round v) vs
        ls = zip (toIntVector vsa) (toIntVector vsb)

        drawLine ((ax:ay:_), (bx:by:_)) = bl_drawLine ax ay bx by >>
                                          bl_drawLine ax (ay + 1) bx (by + 1) 

drawFlSq :: [Vertex] -> Int -> IO ()
drawFlSq (a:b:c:d:[]) col = drawFlTr (a:c:b:[]) col >> drawFlTr (a:c:d:[]) col

drawPolygon :: Polygon -> Screen -> IO ()
drawPolygon p s = drawFlSq (mapPolygon2D p s) (poly_color p)

drawOT :: OrderingTable -> Screen -> IO ()
drawOT o s = mapM_ (\(_,p) -> drawPolygon p s) o

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
