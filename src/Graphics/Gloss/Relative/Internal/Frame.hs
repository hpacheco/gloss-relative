{-# LANGUAGE ViewPatterns #-}

module Graphics.Gloss.Relative.Internal.Frame where

import Graphics.Gloss
import qualified Graphics.Gloss.Rendering as Gloss
import Graphics.Gloss.Relative.Internal.Dimension
import Graphics.Gloss.Relative.Internal.Data
import qualified Graphics.Gloss.Relative.Internal.Picture as Picture
import qualified Graphics.Gloss.Relative.Internal.Window as Window
import qualified Graphics.Gloss.Relative.Internal.Raster as Raster
import qualified Graphics.Gloss.Relative.Internal.Cache as Cache

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Identity (Identity(..))
import qualified Control.Monad.Identity as Identity
import Data.Maybe
import Data.IORef

-- | Renders a frame into a picture. 
renderStaticFrame
    :: Frame -- ^ The frame to render. __Note:__ This function ignores frame labels. Use only if you don't need mouse hover events in this frame.
    -> Dimension -- ^ The dimension of the screen in which to render the frame.
    -> Picture -- ^ The resulting picture.
renderStaticFrame f screen = Window.wPic $ Identity.runIdentity $ Window.execWindow screen (renderStaticFrameAsWindow f)
    where
    renderStaticFrameAsWindow :: Monad m => Frame -> Window.Window m ()
    renderStaticFrameAsWindow (Aspect (aw,ah) a f) = do
        dim <- Window.askDimension
        let adim = Window.largestAspectFitDim aw ah dim
        wo <- lift $ Window.execWindow adim (renderStaticFrameAsWindow f)
        wo' <- lift $ Window.execWindow adim $ Window.fitWith adim wo
        Window.alignWith adim a wo'
    renderStaticFrameAsWindow (Zoom x y a f) = do
        (w,h) <- Window.askDimension
        let dim' = (w*x,h*y)
        wo <- lift $ Window.execWindow dim' (renderStaticFrameAsWindow f)
        Window.alignWith dim' a wo
    renderStaticFrameAsWindow (Grid xs) = do
        Window.grid (map (map renderStaticFrameAsWindow) xs)
        return ()
    renderStaticFrameAsWindow (Frames xs) = mapM_ renderStaticFrameAsWindow xs
    renderStaticFrameAsWindow (Stretch Nothing pic) = Window.stretch pic
    renderStaticFrameAsWindow (Stretch (Just dim) pic) = Window.stretchWith dim pic
    renderStaticFrameAsWindow (Sized f) = do
        dim <- Window.askDimension
        renderStaticFrameAsWindow (f dim)
    renderStaticFrameAsWindow (Label name isTransparent f) = renderStaticFrameAsWindow f -- ignores labels

renderDynamicFrame :: Int -> Cache.CacheTable -> Gloss.State -> IORef (IO Raster.Offscreen) -> Frame -> Dimension -> IO Window.WindowOutput
renderDynamicFrame i cache state off f screen = Window.execWindow screen (renderDynamicFrameAsWindow f)
    where
    renderDynamicFrameAsWindow :: Frame -> Window.Window IO ()
    renderDynamicFrameAsWindow (Aspect (aw,ah) a f) = do
        dim <- Window.askDimension
        let adim = Window.largestAspectFitDim aw ah dim
        wo <- lift $ Window.execWindow adim (renderDynamicFrameAsWindow f)
        wo' <- lift $ Window.execWindow adim $ Window.fitWith adim wo
        Window.alignWith adim a wo'
    renderDynamicFrameAsWindow (Zoom x y a f) = do
        (w,h) <- Window.askDimension
        let dim' = (w*x,h*y)
        wo <- lift $ Window.execWindow dim' (renderDynamicFrameAsWindow f)
        Window.alignWith dim' a wo
    renderDynamicFrameAsWindow (Grid xs) = do
        Window.grid (map (map renderDynamicFrameAsWindow) xs)
        return ()
    renderDynamicFrameAsWindow (Frames xs) = mapM_ renderDynamicFrameAsWindow xs
    renderDynamicFrameAsWindow (Stretch Nothing pic) = Window.stretch pic
    renderDynamicFrameAsWindow (Stretch (Just dim) pic) = Window.stretchWith dim pic
    renderDynamicFrameAsWindow (Sized f) = do
        dim <- Window.askDimension
        renderDynamicFrameAsWindow (f dim)
    renderDynamicFrameAsWindow (Label name False f) = Window.addRegionRectangle name >> renderDynamicFrameAsWindow f
    renderDynamicFrameAsWindow (Label name True f) = Cache.cachedRenderFrame i cache renderLabel name f
        where
        renderLabel :: String -> Frame -> Window.Window IO ()
        renderLabel name f = do
            dim <- Window.askDimension
            let screen = dimensionToScreenSize dim
            o <- liftIO $ Raster.getResizeOffscreen off screen
            (Window.WindowOutput pic region) <- lift $ Window.execWindow dim (renderDynamicFrameAsWindow f)
            pic' <- liftIO $ Raster.rasterPicture state o screen pic
            Window.addRegionTransparent name pic'
            Window.tellWindowOutput (Window.WindowOutput (Raster.fromRasteredPicture pic') region)

-- | Creates each cell in a grid depending on the row and column indexes.
grid :: Int -> Int -> (Int -> Int -> Frame) -> Frame
grid ncols nrows mk = Grid $ map (\row -> map (\col -> mk row col) [0..ncols-1]) [0..nrows-1] 

-- | Alignment to the top-left of the frame.
alignTopLeft :: Alignment
alignTopLeft = RelativeAlignment AlignLeft AlignTop

-- | Alignment to the top (and center) of the frame.
alignTop :: Alignment
alignTop = RelativeAlignment AlignCenter AlignTop

-- | Alignment to the top-right of the frame.
alignTopRight :: Alignment
alignTopRight = RelativeAlignment AlignRight AlignTop

-- | Alignment to the left (and middle) the frame.
alignLeft :: Alignment
alignLeft = RelativeAlignment AlignLeft AlignMiddle

-- | Alignment to the center (and middle) the frame.
alignCenter :: Alignment
alignCenter = RelativeAlignment AlignCenter AlignMiddle

-- | Alignment to the right (and middle) the frame.
alignRight :: Alignment
alignRight = RelativeAlignment AlignRight AlignMiddle

-- | Alignment to the bottom-left the frame.
alignBottomLeft :: Alignment
alignBottomLeft = RelativeAlignment AlignLeft AlignBottom

-- | Alignment to the bottom (and middle) the frame.
alignBottom :: Alignment
alignBottom = RelativeAlignment AlignCenter AlignBottom

-- | Alignment to the bottom-right the frame.
alignBottomRight :: Alignment
alignBottomRight = RelativeAlignment AlignRight AlignBottom

-- | Fit picture to the frame, preserving the picture's aspect ratio. Receives a picture alignment inside the frame.
fit
    :: Maybe Dimension -- ^ The explicit dimension of the picture, or inferred if 'Nothing'.
    -> Alignment -- ^ The alignment of the fitted picture to the current frame.
    -> Picture -- ^ The picture to stretch to the frame's dimension.
    -> Frame -- ^ The resulting frame.
fit mbscreen a pic = Aspect screen a (Stretch (Just picdim) pic)
    where
    picdim = Picture.pictureDimension pic
    screen = fromMaybe picdim mbscreen

-- | Draws a picture inside a frame using the original picture dimensions, with no scaling, stretching or fitting.
-- __Warning:__ May naturally lead to misaligned pictures if not used with care.
absolute :: Picture -> Frame
absolute pic = Sized $ \dim -> Stretch (Just dim) pic

-- | Draws a picture inside a frame using the original picture dimensions, with no scaling, stretching or fitting.
-- Receives a function so that the picture can be created depending on the current frame's size.
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
banner txt c = Sized $ \dim -> 
    let pic = Window.wPic $ Identity.runIdentity $ Window.execWindow dim (Window.text txt)
    in Stretch (Just dim) $ Color c pic

-- | A convex polygon filled with a solid color.
shape
    :: [Point] -- ^ A sequence of points that form the polygon. Differently from Gloss 'polygon', each coordinate of a point @(relx,rely)@ is defined as relative screen width / height percentages between @-0.5@ and @0.5@.
    -> Color -- ^ The fill color.
    -> Frame -- ^ The resulting frame
shape ps c = absoluteSized $ \dim -> Color c $ Polygon $ map (Picture.mulPointwise dim) ps

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
stroke ps c = absoluteSized $ \dim -> Color c $ Line $ map (Picture.mulPointwise dim) ps

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

-- | Draws an ellipsis fitting the current frame.
ellipsis :: Color -> Frame
ellipsis c = absoluteSized $ \dim@(w,h) ->
    let (diameter,sx,sy) = if w < h then (w,1,h/w) else (h,w/h,1)
    in Scale sx sy $ Color c $ Circle (diameter/2)

-- | Draws an ellipsis fitting the current frame, with a given thickness.
thickEllipsis :: Color -> Float -> Frame
thickEllipsis c thick = absoluteSized $ \dim@(w,h) ->
    let (diameter,sx,sy) = if w < h then (w,1,h/w) else (h,w/h,1)
    in Scale sx sy $ Color c $ ThickCircle (diameter/2 - thick/2) thick

-- | Draws an ellipsis fitting the current frame, filled with a given color.
solidEllipsis :: Color -> Frame
solidEllipsis c = absoluteSized $ \dim@(w,h) ->
    let (diameter,sx,sy) = if w < h then (w,1,h/w) else (h,w/h,1)
        radius = diameter / 2
    in Scale sx sy $ Color c $ ThickCircle (radius/2) radius

-- | Zoom from the current frame to a smaller frame with a fixed dimension.
fixedZoom :: Dimension -> Alignment -> Frame -> Frame
fixedZoom (fw,fh) a f = Sized $ \(w,h) ->
    let zx = min 1 (fw / w)
        zy = min 1 (fh / h)
    in Zoom zx zy a f


