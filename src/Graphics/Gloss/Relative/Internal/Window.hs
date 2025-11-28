module Graphics.Gloss.Relative.Internal.Window where
  
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as Gloss
import Graphics.Gloss.Relative.Internal.Dimension
import Graphics.Gloss.Relative.Internal.Picture

import Control.Monad.Reader (Reader(..),ReaderT(..))
import qualified Control.Monad.Reader as Reader
import Control.Monad.Writer (Writer(..),WriterT(..))
import qualified Control.Monad.Writer as Writer
import Control.Monad
import Data.Set (Set(..))
import qualified Data.Set as Set
  
-- * Window data type

-- | A function that returns which regions a position belongs to.
type RegionHandler = Point -> Set String

type WindowOutput = (Picture,RegionHandler)

newtype Window a = Window { unWindow :: ReaderT Dimension (Writer WindowOutput) a }
    deriving (Monad,Applicative,Functor)

mkWindow :: (Dimension -> WindowOutput) -> Window ()
mkWindow f = Window $ do
    screen <- Reader.ask
    Writer.tell $ f screen

runWindow :: Dimension -> Window a -> (a,WindowOutput)
runWindow screen (Window w) = Writer.runWriter (Reader.runReaderT w screen)

execWindow :: Dimension -> Window a -> WindowOutput
execWindow screen w = snd $ runWindow screen w

askDimension :: Window Dimension
askDimension = Window $ Reader.ask

withDimension :: (Dimension -> Dimension) -> Window a -> Window a
withDimension f (Window w) = Window $ Reader.local f w

mapWindowOutput :: (WindowOutput -> WindowOutput) -> Window a -> Window a
mapWindowOutput f (Window w) = Window $ Reader.mapReaderT (Writer.mapWriter (\(x,y) -> (x,f y))) w

emptyWindow :: Window ()
emptyWindow = Window $ Writer.tell (Blank,mempty)

tellWindowOutput :: WindowOutput -> Window ()
tellWindowOutput p = Window $ Writer.tell p

-- * Window transformations
  
-- | Builds a grid of windows, evenly splitting the screen both horiontally and vertically.
grid :: [[Window a]] -> Window [[a]]
grid = rows . map columns

-- | Evenly splits the screen vertically into a list.
rows :: [Window a] -> Window [a]
rows ws = Window $ do
    dim@(dimx,dimy) <- Reader.ask
    let dimyn = (realToFrac dimy) / (realToFrac $ length ws)
    let go [] = fmap (Prelude.const []) emptyWindow
        go (x:xs) = fmap (uncurry (:)) (top dimyn x (go xs))
    unWindow $ go ws

-- | Evenly splits the screen horizontally into a list.
columns :: [Window a] -> Window [a]
columns ws = Window $ do
    dim@(dimx,dimy) <- Reader.ask
    let dimxn = (realToFrac dimx) / (realToFrac $ length ws)
    let go [] = fmap (Prelude.const []) emptyWindow
        go (x:xs) = fmap (uncurry (:)) (left dimxn x (go xs))
    unWindow $ go ws

-- | Top-biased vertical composition of two windows. Receives height of top row.
top :: Float -> Window a -> Window b -> Window (a,b)
top sy1 w1 w2  = Window $ do
    dim@(sx,sy) <- Reader.ask
    let sy2 = sy - sy1
    let (a,(p1,r1)) = runWindow (sx,sy1) w1
    let (b,(p2,r2)) = runWindow (sx,sy2) w2
    let p12 = Pictures [translateY (realToFrac sy2/2) p1,translateY (-realToFrac sy1/2) p2]
    let r12 pos = Set.unions [r1 $ translatePointY (-realToFrac sy2/2) pos, r2 $ translatePointY (realToFrac sy1/2) pos]
    Writer.tell (p12,r12)
    return (a,b)

-- | Bottom-biased vertical composition of two windows. Receives height of bottom row.
bottom :: Float -> Window a -> Window b -> Window (a,b)
bottom sy2 w1 w2 = do
    (w,h) <- askDimension
    top (h-sy2) w1 w2

-- Left-biased horizontal composition of two windows. Receives width of left column.
left :: Float -> Window a -> Window b -> Window (a,b)
left sx1 w1 w2 = Window $ do
    (sx,sy) <- Reader.ask
    let sx2 = sx - sx1
    let (a,(p1,r1)) = runWindow (sx1,sy) w1
    let (b,(p2,r2)) = runWindow (sx2,sy) w2
    let p12 = Pictures [translateX (-realToFrac sx2/2) p1,translateX (realToFrac sx1/2) p2]
    let r12 pos = Set.unions [r1 $ translatePointX (realToFrac sx2/2) pos,r2 $ translatePointX (-realToFrac sx1/2) pos]
    Writer.tell (p12,r12)
    return (a,b)

-- Right-biased horizontal composition of two windows. Receives width of right column.
right :: Float -> Window a -> Window b -> Window (a,b)
right sx2 w1 w2 = do
    (w,h) <- askDimension
    left (w-sx2) w1 w2

-- | Stretches a picture to fit the window
stretchWith :: Dimension -> Picture -> Window ()
stretchWith (cx,cy) pic = Window $ do
    screen@(sx,sy) <- Reader.ask
    let scalex = max 0 (realToFrac sx / realToFrac cx)
        scaley = max 0 (realToFrac sy / realToFrac cy)
    Writer.tell (Scale scalex scaley pic,mempty)
    return ()

stretch :: Picture -> Window ()
stretch pic = stretchWith (pictureDimension pic) pic

align :: Alignment -> Dimension -> Dimension -> (Float,Float)
align (Alignment ha va) c s = halign ha c s Gloss.+ valign va c s
    
halign :: HorizontalAlignment -> Dimension -> Dimension -> (Float,Float)
halign a (cx,cy) (sx,sy) = case a of
    AlignLeft   -> (-((sx-cx)/2),0)
    AlignRight  -> (((sx-cx)/2),0)
    AlignCenter -> (0,0)

valign :: VerticalAlignment -> Dimension -> Dimension -> (Float,Float)
valign a (cx,cy) (sx,sy) = case a of
    AlignTop    -> (0,((sy-cy)/2))
    AlignBottom -> (0,-((sy-cy)/2))
    AlignMiddle -> (0,0)

alignWith :: Dimension -> Alignment -> WindowOutput -> Window ()
alignWith dim a (pic,region) = Window $ do
    screen <- Reader.ask
    let (ax,ay) = align a dim screen
    let pic' = translateX ax $ translateY ay pic
    let region' pos = region $ translatePointX (-ax) $ translatePointY (-ay) pos
    Writer.tell (pic',region')

fitWith :: Dimension -> WindowOutput -> Window ()
fitWith (cx,cy) (pic,region) = Window $ do
    (sx,sy) <- Reader.ask
    let scalex = sx / cx
    let scaley = sy / cy
    let scalexy = max 0 (min scalex scaley)
    let dim' = (cx*scalexy,cy*scalexy)
    let pic' = Scale scalexy scalexy pic
    let region' pos = region $ scalePoint (1/scalexy) (1/scalexy) pos
    Writer.tell (pic',region')

fit :: WindowOutput -> Window ()
fit (pic,region) = fitWith (pictureDimension pic) (pic,region)

largestAspectFit :: Float -> Float -> Dimension -> Dimension
largestAspectFit a b (screenW,screenH)
  | screenW / screenH >= a / b = (screenH * (a / b), screenH)
  | otherwise                  = (screenW, screenW * (b / a))

text :: String -> Window ()
text str = fitWith dim (pic,mempty)
    where
    textWidth = realToFrac (length str) * (realToFrac charWidth)
    pic = Translate (-realToFrac textWidth/2) (-realToFrac charHeight/2) $ Text str
    dim = (textWidth,realToFrac charHeight)

-- * Regions

-- | Registers a new region for the current window
addRegion :: String -> Window ()
addRegion name = do
    dim <- askDimension
    let reg = dimensionToRegion dim
    tellWindowOutput (Blank,\p -> if pointInsideRegion p reg then Set.singleton name else Set.empty)

-- * Alignment types

-- | Alignment options for a picture or frame inside a larger frame.
data Alignment = Alignment
  { alignH :: HorizontalAlignment -- ^ Horizontal alignment.
  , alignV :: VerticalAlignment -- ^ Vertical alignment.
  } deriving (Eq,Ord,Show)

-- | Horizontal alignment options for a picture or frame inside a larger frame.
data HorizontalAlignment
    = AlignLeft
    | AlignCenter  
    | AlignRight
    deriving (Eq,Ord,Show,Enum,Bounded)

-- | Vertical alignment options for a picture or frame inside a larger frame.
data VerticalAlignment
    = AlignTop
    | AlignMiddle  
    | AlignBottom
    deriving (Eq,Ord,Show,Enum,Bounded)


