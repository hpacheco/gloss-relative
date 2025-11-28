module Graphics.Gloss.Relative.Internal.Picture where

import Graphics.Gloss
import Graphics.Gloss.Relative.Internal.Dimension

import Data.Semigroup
import Data.Monoid

-- * Pictures

translateX :: Float -> Picture -> Picture
translateX x p = Translate x 0 p

translateY :: Float -> Picture -> Picture
translateY y p = Translate 0 y p

-- * Regions

-- | A rectangular region within the screen.
data Region = Region
    { regionLeftTop :: Point
    , regionDimension :: Dimension
    } deriving (Eq,Ord,Show)

instance Semigroup Region where
    (Region (x1,y1) (w1,h1)) <> (Region (x2,y2) (w2,h2)) = Region (xmin,ymin) (abs (xmax-xmin),abs (ymax-ymin))
         where xmin = min x1 x2
               ymin = min y1 y2
               xmax = max (x1+w1) (x2+w2)
               ymax = max (y1+h1) (y2+h2)

instance Monoid Region where
    mempty = Region (0,0) (0,0)

regionScreenSize :: Region -> ScreenSize
regionScreenSize (Region (x,y) (xdim,ydim)) = (ceiling $ maxX * 2,ceiling $ maxY * 2)
    where
    maxX = max (abs x) (abs $ x + xdim)
    maxY = max (abs y) (abs $ y + ydim)

dimensionToRegion :: Dimension -> Region
dimensionToRegion (w,h) = Region { regionLeftTop = (-w/2,h/2), regionDimension = (w,h) }

pointInsideRegion :: Point -> Region -> Bool
pointInsideRegion (mouseX,mouseY) r =
    let (regionLeft,regionTop) = regionLeftTop r in
    let (w,h) = regionDimension r in
    let regionRight = regionLeft + w in
    let regionBottom = regionTop - h in
    (regionLeft <= mouseX && mouseX <= regionRight) && (regionBottom <= mouseY && mouseY <= regionTop)

translateRegionX :: Float -> Region -> Region
translateRegionX f r = r { regionLeftTop = translatePointX f (regionLeftTop r) }

translateRegionY :: Float -> Region -> Region
translateRegionY f r = r { regionLeftTop = translatePointY f (regionLeftTop r) }

-- * Picture bounding box

-- | Estimates the 'Region' of a 'Picture'.
pictureRegion :: Picture -> Region
pictureRegion = bbToRegion . bbox

pictureScreenSize :: Picture -> ScreenSize
pictureScreenSize = regionScreenSize . pictureRegion 

pictureDimension :: Picture -> Dimension
pictureDimension = screenSizeToDimension . pictureScreenSize

type Elipsis = (Float,Float,Float,Float,Float,Float)
data BB = BB { bbElipsis :: [Elipsis], bbPoints :: [Point] } deriving Show

instance Semigroup BB where
    (BB es1 ps1) <> (BB es2 ps2) = BB (es1++es2) (ps1++ps2)
instance Monoid BB where
    mempty = BB [] []

bbox :: Picture -> BB
bbox Blank = mempty
bbox (Polygon l) = BB [] l
bbox (Line l) = BB [] l
bbox (Circle r) = case (circElipsis r) of
    Nothing -> mempty
    Just e -> BB [e] []
bbox (ThickCircle r thick) = case (circElipsis $ r + thick / 2) of
    Nothing -> mempty
    Just e -> BB [e] []
bbox (Arc _ _ r) = bbox (Circle r) -- big overapproximation
bbox (ThickArc _ _ r thick) = bbox (ThickCircle r thick) -- big overapproximation
bbox (Text str) = bbox $ Translate (textWidth/2) (fromIntegral charHeight/2) rect
    where
    rect = rectangleWire textWidth (realToFrac charHeight)
    textWidth = realToFrac (length str) * (realToFrac charWidth)
bbox (Bitmap bmp) = bbox $ rectangleWire (fromIntegral w) (fromIntegral h)
    where (w,h) = bitmapSize bmp
bbox (BitmapSection rect bmp) = bbox $ rectangleWire (fromIntegral w) (fromIntegral h)
    where (w,h) = rectSize rect
bbox (Color c p) = bbox p
bbox (Scale x y p) = scaleBB x y (bbox p)
bbox (Translate x y p) = translateBB x y (bbox p)
bbox (Rotate alpha p) = rotateBB alpha (bbox p)
bbox (Pictures xs) = mconcat $ map bbox xs

charHeight :: Int
charHeight =  104
charWidth :: Int
charWidth = 70

scaleBB :: Float -> Float -> BB -> BB
scaleBB x y (BB es ps) = BB (map (scaleElipsis x y) es) (scalePath x y ps)

translateBB :: Float -> Float -> BB -> BB
translateBB x y (BB es ps) = BB (map (translateElipsis x y) es) (translatePath x y ps)

rotateBB :: Float -> BB -> BB
rotateBB alpha (BB es ps) = BB (map (rotateElipsis alpha) es) (rotatePath alpha ps)

bbToRegion :: BB -> Region
bbToRegion (BB es ps) = mconcat $ pathRegion ps : map elipsisRegion es

pathRegion :: Path -> Region
pathRegion [] = mempty
pathRegion l = Region (xmin,ymin) (abs (xmax-xmin),abs (ymax-ymin))
 where (xs,ys) = unzip l
       xmin = minimum xs
       ymin = minimum ys
       xmax = maximum xs
       ymax = maximum ys

elipsisRegion :: Elipsis -> Region
elipsisRegion (a,b,c,d,e,f) | dlt==0 = mempty
                        | otherwise = Region (xl,yb) (abs (xr-xl),abs (yt-yb))
 where dlt = 4*a*c - b^2
       xc = (b*e - 2*c*d) / dlt
       yc = (b*d - 2*a*e) / dlt
       xr = xc + sqrt ((2*b*e-4*c*d)^2+4*dlt*(e^2-4*c*f)) / (2*dlt)
       yt = yc + sqrt ((2*b*d-4*a*e)^2+4*dlt*(d^2-4*a*f)) / (2*dlt)
       xl = xc - sqrt ((2*b*e-4*c*d)^2+4*dlt*(e^2-4*c*f)) / (2*dlt)
       yb = yc - sqrt ((2*b*d-4*a*e)^2+4*dlt*(d^2-4*a*f)) / (2*dlt)

-- converts point from cartesian to polar coordinates
toPolar :: Point -> (Float,Float)
toPolar (x,y) = (sqrt (x^2+y^2), atan2 y x)

-- converts point from polar to cartesian
toCartesian :: (Float,Float) -> Point
toCartesian (r,gama) = (r*cos gama,r*sin gama)

-- a circle as an elipsis
circElipsis :: Float -> Maybe Elipsis
circElipsis 0 = Nothing
circElipsis r = Just (1/r^2,0,1/r^2,0,0,-1)

rotatePoint :: Float -> Point -> Point
rotatePoint alpha (x,y) = toCartesian (r,gama + (rad (-alpha)))
 where (r,gama) = toPolar (x,y)
 
rotatePath :: Float -> Path -> Path
rotatePath alpha = map (rotatePoint alpha)

rotateElipsis :: Float -> Elipsis -> Elipsis
rotateElipsis alpha (a,b,c,d,e,f) = (a',b',c',d',e',f')
 where theta = rad (alpha)
       a' = a*(cos theta)^2 + b*sin theta*cos theta + c*(sin theta)^2
       b' = 2*(c-a)*sin theta*cos theta + b*((cos theta)^2-(sin theta)^2)
       c' = a*(sin theta)^2 - b*sin theta*cos theta + c*(cos theta)^2
       d' = d*cos theta + e*sin theta
       e' = -d*sin theta + e*cos theta
       f' = f

translatePoint :: Float -> Float -> Point -> Point
translatePoint x y (a,b) = (a+x,b+y)

translatePointX :: Float -> Point -> Point
translatePointX f (x,y) = (x+f,y)

translatePointY :: Float -> Point -> Point
translatePointY f (x,y) = (x,y+f)

translatePath :: Float -> Float -> Path -> Path
translatePath x y = map (translatePoint x y)

translateElipsis :: Float -> Float -> Elipsis -> Elipsis
translateElipsis x y (a,b,c,d,e,f) = (a',b',c',d',e',f')
 where a' = a
       b' = b
       c' = c
       d' = d - 2*a*x - b*y
       e' = e - b*x - 2*c*y
       f' = f + a*x^2 + b*x*y + c*y^2 - d*x -e*y

scalePoint :: Float -> Float -> Point -> Point
scalePoint xs ys (x,y) = (x*xs,y*ys)

scalePath :: Float -> Float -> Path -> Path
scalePath xs ys = map (scalePoint xs ys)

scaleElipsis :: Float -> Float -> Elipsis -> Elipsis
scaleElipsis xs ys (a,b,c,d,e,f) = (a',b',c',d',e',f')
 where a' = a / (xs^2)
       b' = b / (xs * ys)
       c' = c / (ys^2)
       d' = d / xs
       e' = e / ys
       f' = f

mulPointwise :: Point -> Point -> Point
mulPointwise (a,b) (c,d) = (a*c,b*d)

-- * Auxiliary functions

-- | Converts degrees to radians
rad :: Float -> Float
rad alpha = alpha * pi / 180

-- | Converts radians to degrees
deg :: Float -> Float
deg alpha = alpha * 180 / pi
