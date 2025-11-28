{-# LANGUAGE ViewPatterns #-}

-- | The core functionality of the @gloss-relative@ package is exported in this module.
-- This includes the 'Frame' abstraction and helper functions.
module Graphics.Gloss.Relative.Frame
    ( Frame(..), Dimension(..)
    , renderStaticFrame, grid, fit, wire, solid, banner, shape, stroke, bordered, absolute, absoluteSized
    , Alignment(..), HorizontalAlignment(..), VerticalAlignment(..)
    , alignTopLeft, alignTop, alignTopRight, alignLeft, alignCenter, alignRight, alignBottomLeft, alignBottom, alignBottomRight
    ) where

import Graphics.Gloss.Relative.Internal.Dimension 
import Graphics.Gloss.Relative.Internal.Frame 
import Graphics.Gloss.Relative.Internal.Window hiding (grid,fit)

