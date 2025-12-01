-- | Various game modes using frames instead of pictures.
module Graphics.Gloss.Relative.Interface
        ( module Graphics.Gloss.Data.Display
        , module Graphics.Gloss.Data.Picture
        , module Graphics.Gloss.Data.Color
        , displayRelative, displayRelativeIO, playRelative, playRelativeIO
        , Controller(..), Event(..), Mouse(..), Key(..), SpecialKey(..), MouseButton(..), KeyState(..), Modifiers(..))
where
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Relative.Frame
import Graphics.Gloss.Relative.Internal.Dimension
import qualified Graphics.Gloss.Relative.Internal.Picture as Picture
import qualified Graphics.Gloss.Relative.Internal.Window as Window
import qualified Graphics.Gloss.Relative.Internal.Frame as Frame
import qualified Graphics.Gloss.Relative.Internal.Raster as Raster
import qualified Graphics.Gloss.Relative.Internal.Cache as Cache
import qualified Graphics.Gloss.Interface.Pure.Display as Gloss
import qualified Graphics.Gloss.Interface.Pure.Game as Gloss
import Graphics.Gloss.Interface.Pure.Game (Key(..), SpecialKey(..), MouseButton(..), KeyState(..), Modifiers(..))
import qualified Graphics.Gloss.Interface.IO.Display as Gloss
import Graphics.Gloss.Interface.IO.Display (Controller(..))
import qualified Graphics.Gloss.Interface.IO.Game as Gloss
import qualified Graphics.Gloss.Rendering as Gloss

import Control.Monad
import qualified Data.Set as Set
import Data.IORef
import System.Exit

-- | A custom Gloss event type that knows about frame regions.
data Event
    -- | A key press.
    = EventKey { eventKey :: Key, eventKeyState :: KeyState, eventModifiers :: Modifiers, eventMouse :: Mouse }
    -- | A mouse movement.
    | EventMotion { eventMouse :: Mouse }
    deriving (Eq, Show)

-- | Information about the mouse.
data Mouse = Mouse
    { mousePosition :: Point -- ^ Current mouse position.
    , mouseInside :: [String] -- ^ List of regions in which the mouse is currently inside. __Note:__ The exact regions correspond to the labels defined in the current 'Frame'.
    } deriving (Eq,Show)

fromGlossEvent :: Gloss.Event -> Window.RegionHandler -> IO (Either Event Dimension)
fromGlossEvent (Gloss.EventMotion pos) region = do
    rs <- region pos
    return $ Left $ EventMotion (Mouse pos $ Set.toList rs)
fromGlossEvent (Gloss.EventKey k st m pos) region = do
    rs <- region pos
    return $ Left $ EventKey k st m (Mouse pos $ Set.toList rs)
fromGlossEvent (Gloss.EventResize screen) region = do
    return $ Right (screenSizeToDimension screen)

-- | A variant of 'Gloss.display' using 'Frame'.
displayRelative
    :: Display          -- ^ Display mode.
    -> Color            -- ^ Background color.
    -> Frame            -- ^ The frame to draw.
    -> IO ()
displayRelative dis backColor frame = do
    screen <- getDisplayDimension dis
    let pic = renderStaticFrame frame screen
    Gloss.display dis backColor pic

-- | A variant of 'Gloss.displayIO' using 'Frame'.
displayRelativeIO
        :: Display                -- ^ Display mode.
        -> Color                  -- ^ Background color.
        -> IO Frame               -- ^ Action to produce the current frame.
        -> (Controller -> IO ())  -- ^ Callback to take the display controller.
        -> IO ()

displayRelativeIO dis backColor makeFrame eatController = do
    screen <- getDisplayDimension dis
    let makePicture = do
            frame <- makeFrame
            let pic = renderStaticFrame frame screen
            return pic
    Gloss.displayIO dis backColor makePicture eatController

-- | A variant of 'Gloss.play' using 'Frame'. The resulting picture is automatically redimensioned on resize events.
playRelative
    :: Display              -- ^ Display mode.
    -> Color                -- ^ Background color.
    -> Int                  -- ^ Number of simulation steps to take for each second of real time.
    -> world                -- ^ The initial world.
    -> (world -> Frame)     -- ^ A function to convert the world a picture.
    -> (Event -> world -> world)
            -- ^ A function to handle input events.
    -> (Float -> world -> world)
            -- ^ A function to step the world one iteration.
            --   It is passed the period of time (in seconds) needing to be advanced.
    -> IO ()
playRelative display backColor simResolution worldStart worldToFrame worldHandleEvent worldAdvance = do
    let handleEvent (EventKey (Gloss.SpecialKey Gloss.KeyEsc) Gloss.Down _ _) st = exitSuccess
        handleEvent ev st = return $ worldHandleEvent ev st
    playRelativeIO display backColor simResolution worldStart (return . worldToFrame) handleEvent (\x -> return . worldAdvance x)

-- | A variant of 'Gloss.playIO' using 'Frame'. The resulting picture is automatically redimensioned on resize events.
playRelativeIO
    :: Display              -- ^ Display mode.
    -> Color                -- ^ Background color.
    -> Int                  -- ^ Number of simulation steps to take for each second of real time.
    -> world                -- ^ The initial world.
    -> (world -> IO Frame)  -- ^ A function to convert the world a picture.
    -> (Event -> world -> IO world)
            -- ^ A function to handle input events.
    -> (Float -> world -> IO world)
            -- ^ A function to step the world one iteration.
            --   It is passed the period of time (in seconds) needing to be advanced.
    -> IO ()
playRelativeIO display backColor simResolution worldStart worldToFrame worldHandleEvent worldAdvance = do
    handler :: IORef Window.RegionHandler <- newIORef mempty
    screen <- getDisplayScreenSize display
    let dim = screenSizeToDimension screen
    -- independent gloss state for parallel rendering
    glossState <- Gloss.initState
    cache <- Cache.newCacheTable
    currentFrame :: IORef Int <- newIORef 0 -- wraps around
    offscreen :: IORef (IO Raster.Offscreen) <- newIORef (Raster.createOffscreen screen)
    let draw (w,s) = do
            i <- readIORef currentFrame
            f <- worldToFrame w
            (Window.WindowOutput pic h) <- Frame.renderDynamicFrame i cache glossState offscreen f s
            writeIORef handler $! h
            writeIORef currentFrame $! i + 1
            Cache.evictOldCacheTable (i+1) simResolution cache
            return pic
    let handleEvent ev (w,s) = do
            h <- readIORef handler
            fromGlossEvent ev h >>= \e -> case e of
                Left ev' -> liftM (,s) (worldHandleEvent ev' w)
                Right s' -> return (w,s')
    let advance time (w,s) = liftM (,s) (worldAdvance time w)
    Gloss.playIO display backColor simResolution (worldStart,dim) draw handleEvent advance

