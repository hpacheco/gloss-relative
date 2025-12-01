module Graphics.Gloss.Relative.Internal.Data where

import Graphics.Gloss
import Graphics.Gloss.Relative.Internal.Dimension

-- | A picture frame. Much like the original 'Picture' data type, but the purpose is to place and adjust pictures inside a frame with general dimensions.
data Frame
    -- | Create the largest possible frame with a desired aspect ratio within the current frame.
    = Aspect
        { aspectRatio :: Dimension -- ^ Aspect ratio dimensions.
        , aspectAlign :: Alignment -- ^ Alignment inside the parent frame.
        , aspectChild :: Frame -- ^ Child frame.
        } 
    -- | Zoom the current frame by given factors, producing a smaller frame.
    | Zoom
        { zoomX :: Float -- ^ Horizontal scale (percentage between 0 and 1).
        , zoomY :: Float -- ^ Vertical scale (percentage between 0 and 1).
        , zoomAlignment :: Alignment -- ^ Alignment inside the parent frame.
        , zoomChild :: Frame -- ^ Child frame.
        }
    -- | Split the current frame into a grid with the given numbers of rows and columns. Receives a matrix of frames, represented as a list of rows.
    | Grid [[Frame]]
    -- | Labels a frame region, to use in mouse events.
    | Label
        { labelName :: String -- ^ The label for the frame region. Does not need to be unique.
        , labelTransparent :: Bool -- ^ If 'True', transparent pixels do not count for frame region selection. If 'False', the whole rectangular region is used for selection. __Note:__ Use transparency with care, ideally only for smaller regions, as performance may suffer significantly.
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

-- * Alignments

-- | Alignment options for a picture or frame inside a larger frame.
data Alignment
    -- | Alignment of a child frame relative to its parent frame.
    = RelativeAlignment
        { alignH :: HorizontalAlignment -- ^ Horizontal alignment.
        , alignV :: VerticalAlignment -- ^ Vertical alignment.
        }
    -- | Advanced contructor. Absolute offset in pixels of the center of the child frame relative to its parent frame.
    -- __Note:__: The offset is truncated, so that the child frame will always remain within the parent frame.
    | AbsoluteAlignment
        { alignPos :: Point -- ^ Alignment position of the center of a child frame inside a parent frame.
        }
    deriving (Eq,Ord,Show)

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