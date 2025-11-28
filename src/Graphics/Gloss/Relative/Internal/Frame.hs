{-# LANGUAGE ViewPatterns #-}

module Graphics.Gloss.Relative.Internal.Frame where

import Graphics.Gloss
import Graphics.Gloss.Relative.Internal.Dimension
import qualified Graphics.Gloss.Relative.Internal.Picture as Relative
import qualified Graphics.Gloss.Relative.Internal.Window as Relative
import Graphics.Gloss.Relative.Internal.Window (Alignment(..), HorizontalAlignment(..), VerticalAlignment(..))

import Control.Monad
import Data.Maybe

-- | A picture frame. Much like the original 'Picture' data type, but the purpose is to place and adjust pictures inside a frame with general dimensions.
data Frame
    -- | Create the largest possible frame with a desired aspect ratio within the current frame.
    = Aspect
        { aspectRatio :: Dimension -- ^ Aspect ratio dimensions.
        , aspectAlign :: Relative.Alignment -- ^ Alignment inside the parent frame.
        , aspectChild :: Frame -- ^ Child frame.
        } 
    -- | Zoom the current frame by given factors, producing a smaller frame.
    | Zoom
        { zoomX :: Float -- ^ Horizontal scale (percentage between 0 and 1).
        , zoomY :: Float -- ^ Vertical scale (percentage between 0 and 1).
        , zoomAlignment :: Relative.Alignment -- ^ Alignment inside the parent frame.
        , zoomChild :: Frame -- ^ Child frame.
        }
    -- | Split the current frame into a grid with the given numbers of rows and columns. Receives a matrix of frames, represented as a list of rows.
    | Grid [[Frame]]
    -- | Labels a frame region, to use in mouse events.
    | Label
        { labelName :: String -- ^ The label for the frame region. Does not need to be unique.
        , labelChild :: Frame -- ^ Current frame.
        }
    -- | Overlay a sequence of frames.
    | Frames [Frame]
    -- | Stretch picture to fill the frame, not preserving the picture's aspect ratio. If you want to preserve the aspect ratio, consider using 'fit' instead.
    | Stretch
        { stretchDimension :: Maybe Dimension -- ^ The explicit dimension of the picture, or inferred if 'Nothing'.
        , stretchPicture :: Picture -- ^ The picture to stretch to the frame's dimension.
        }
    -- | Advanced contructor. In case you need to know the exact screen size for a frame.
    | Sized (Dimension -> Frame)

-- | Renders a frame into a picture. 
renderStaticFrame
    :: Frame -- ^ The frame to render. __Note:__ This function ignores frame labels. Use only if you don't need mouse hover events in this frame.
    -> Dimension -- ^ The dimension of the screen in which to render the frame.
    -> Picture -- ^ The resulting picture.
renderStaticFrame f screen = fst $ Relative.execWindow screen (renderFrameAsWindow f)

renderDynamicFrame :: Frame -> Dimension -> Relative.WindowOutput
renderDynamicFrame f screen = Relative.execWindow screen (renderFrameAsWindow f)

renderFrameAsWindow :: Frame -> Relative.Window ()
renderFrameAsWindow (Aspect (aw,ah) a f) = do
    dim <- Relative.askDimension
    let adim = Relative.largestAspectFit aw ah dim
    let wo = Relative.execWindow adim (renderFrameAsWindow f)
    let wo' = Relative.execWindow adim $ Relative.fitWith adim wo
    Relative.alignWith adim a wo'
renderFrameAsWindow (Zoom x y a f) = do
    (w,h) <- Relative.askDimension
    let dim' = (w*x,h*y)
    let wo = Relative.execWindow dim' (renderFrameAsWindow f)
    Relative.alignWith dim' a wo
renderFrameAsWindow (Grid xs) = do
    Relative.grid (map (map renderFrameAsWindow) xs)
    return ()
renderFrameAsWindow (Frames xs) = mapM_ renderFrameAsWindow xs
renderFrameAsWindow (Stretch Nothing pic) = Relative.stretch pic
renderFrameAsWindow (Stretch (Just dim) pic) = Relative.stretchWith dim pic
renderFrameAsWindow (Sized f) = do
    dim <- Relative.askDimension
    renderFrameAsWindow (f dim)
renderFrameAsWindow (Label name f) = do
    Relative.addRegion name
    renderFrameAsWindow f

-- | Creates each cell in a grid depending on the row and column indexes.
grid :: Int -> Int -> (Int -> Int -> Frame) -> Frame
grid ncols nrows mk = Grid $ map (\row -> map (\col -> mk row col) [0..ncols-1]) [0..nrows-1] 

-- | Alignment to the top-left of the frame.
alignTopLeft :: Alignment
alignTopLeft = Alignment AlignLeft AlignTop

-- | Alignment to the top (and center) of the frame.
alignTop :: Alignment
alignTop = Alignment AlignCenter AlignTop

-- | Alignment to the top-right of the frame.
alignTopRight :: Alignment
alignTopRight = Alignment AlignRight AlignTop

-- | Alignment to the left (and middle) the frame.
alignLeft :: Alignment
alignLeft = Alignment AlignLeft AlignMiddle

-- | Alignment to the center (and middle) the frame.
alignCenter :: Alignment
alignCenter = Alignment AlignCenter AlignMiddle

-- | Alignment to the right (and middle) the frame.
alignRight :: Alignment
alignRight = Alignment AlignRight AlignMiddle

-- | Alignment to the bottom-left the frame.
alignBottomLeft :: Alignment
alignBottomLeft = Alignment AlignLeft AlignBottom

-- | Alignment to the bottom (and middle) the frame.
alignBottom :: Alignment
alignBottom = Alignment AlignCenter AlignBottom

-- | Alignment to the bottom-right the frame.
alignBottomRight :: Alignment
alignBottomRight = Alignment AlignRight AlignBottom

-- | Fit picture to the frame, preserving the picture's aspect ratio. Receives a picture alignment inside the frame.
fit
    :: Maybe Dimension -- ^ The explicit dimension of the picture, or inferred if 'Nothing'.
    -> Relative.Alignment -- ^ The alignment of the fitted picture to the current frame.
    -> Picture -- ^ The picture to stretch to the frame's dimension.
    -> Frame -- ^ The resulting frame.
fit mbscreen a pic = Aspect screen a (Stretch (Just picdim) pic)
    where
    picdim = Relative.pictureDimension pic
    screen = fromMaybe picdim mbscreen

-- | Draws a picture inside a frame using the original picture dimensions, with no scaling, stretching or fitting.
-- __Warning:__ May naturally lead to misaligned pictures if not used with care.
absolute :: Picture -> Frame
absolute pic = Sized $ \dim -> Stretch (Just dim) pic

-- | Draws a picture inside a frame using the original picture dimensions, with no scaling, stretching or fitting.
-- Receives a function so that the picture can created depending on the current frame's size.
-- __Warning:__ May naturally lead to misaligned pictures if not used with care.
absoluteSized :: (Dimension -> Picture) -> Frame
absoluteSized fpic = Sized $ \dim -> Stretch (Just dim) (fpic dim)

-- | Paints the borders of the frame with a color.
wire :: Color -> Frame
wire c = Stretch Nothing $ Color c $ rectangleWire 10 10 -- any dimension would work here

-- | Paints the frame with a solid color.
solid :: Color -> Frame
solid c = Stretch Nothing $ Color c $ rectangleSolid 10 10 -- any dimension would work here

-- | Draws a banner, that is, a piece of text fitted inside the frame, with a color.
banner :: String -> Color -> Frame
banner txt c = Sized $ \dim -> fit Nothing alignCenter $ Color c $ fst $ Relative.execWindow dim (Relative.text txt)

-- | A convex polygon filled with a solid color.
shape
    :: [Point] -- ^ A sequence of points that form the polygon. Differently from Gloss 'polygon', each coordinate of a point @(relx,rely)@ is defined as relative screen width / height percentages between @-0.5@ and @0.5@.
    -> Color -- ^ The fill color.
    -> Frame -- ^ The resulting frame
shape ps c = absoluteSized $ \dim -> Color c $ Polygon $ map (Relative.mulPointwise dim) ps

-- | embedding of Gloss 'polygon' with absolute sizes.
absoluteShape
    :: [Point] 
    -> Color 
    -> Frame 
absoluteShape ps c = absoluteSized $ \dim -> Color c $ Polygon ps

-- | A line connecting a sequence of points, drawn with a color.
stroke
    :: [Point] -- ^ A sequence of points that form the polygon. Differently from Gloss 'polygon', each coordinate of a point @(relx,rely)@ is defined as relative screen width / height percentages between @-0.5@ and @0.5@.
    -> Color -- ^ The fill color.
    -> Frame -- ^ The resulting frame
stroke ps c = absoluteSized $ \dim -> Color c $ Line $ map (Relative.mulPointwise dim) ps

-- | Draws a border around a smaller frame.
bordered
    :: Float -- ^ A fixed thickness in pixels.
    -> Color -- ^ The color for the border.
    -> Frame -- ~ The inner frame, whose screen dimension is smaller by the defined thickness.
    -> Frame -- ^ The resulting frame.
bordered thick c frame = Sized $ \dim@(w,h) ->
    let w' = max 0 (w-thick)
        h' = max 0 (h-thick)
        borderleft = absoluteShape [(-w/2,-h/2),(-w/2,h/2),(-w'/2,h/2),(-w'/2,-h/2)] c
        borderright = absoluteShape [(w/2,-h/2),(w/2,h/2),(w'/2,h/2),(w'/2,-h/2)] c
        bordertop = absoluteShape [(-w/2,h/2),(w/2,h/2),(w/2,h'/2),(-w/2,h'/2)] c
        borderbottom = absoluteShape [(-w/2,-h/2),(w/2,-h/2),(w/2,-h'/2),(-w/2,-h'/2)] c
        borders = [borderleft,borderright,bordertop,borderbottom]
        inner = Zoom (w' / w) (h' / h) alignCenter frame
    in Frames $ borders ++ [inner]

