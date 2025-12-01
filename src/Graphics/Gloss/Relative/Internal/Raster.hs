module Graphics.Gloss.Relative.Internal.Raster where

import Graphics.Gloss.Relative.Internal.Dimension
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Gloss.Rendering as Gloss
import Graphics.Gloss
import Data.ByteString.Internal (create)
import Foreign.Ptr (Ptr)
import Data.Word (Word8)
import Foreign
import Foreign.ForeignPtr
import Data.Word
import Data.IORef
import Control.Monad

data Offscreen = Offscreen
    { offFBO  :: GL.FramebufferObject
    , offTex  :: GL.TextureObject
    , offDim  :: ScreenSize
    }

createOffscreen :: ScreenSize -> IO Offscreen
createOffscreen dim@(w,h) = do
    fbo <- GL.genObjectName
    GL.bindFramebuffer GL.Framebuffer $= fbo
    
    tex <- GL.genObjectName
    GL.textureBinding GL.Texture2D $= Just tex
    GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA8 (GL.TextureSize2D (fromIntegral w) (fromIntegral h)) 0 (GL.PixelData GL.RGBA GL.UnsignedByte nullPtr)
    
    GL.framebufferTexture2D GL.Framebuffer (GL.ColorAttachment 0) GL.Texture2D tex 0
    
    GL.drawBuffer $= GL.FBOColorAttachment 0
    GL.readBuffer $= GL.FBOColorAttachment 0
    
    status <- GL.get (GL.framebufferStatus GL.Framebuffer)
    when (status /= GL.Complete) $ error $ "Offscreen FBO incomplete " ++ show status
    
    GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject
    return Offscreen { offFBO = fbo, offTex = tex, offDim = dim }

deleteOffscreen :: Offscreen -> IO ()
deleteOffscreen off = do
  -- Unbind first
  GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject
  GL.textureBinding GL.Texture2D $= Nothing

  -- Delete GPU objects
  GL.deleteObjectName (offFBO off)
  GL.deleteObjectName (offTex off)

getResizeOffscreen :: IORef (IO Offscreen) -> ScreenSize -> IO Offscreen
getResizeOffscreen r (w,h) = do
    moff <- readIORef r
    off <- moff
    let (offw,offh) = offDim off
    off' <- if w > offw || h > offh
        then do
            deleteOffscreen off
            off' <- createOffscreen (w,h)
            return off'
        else return off
    writeIORef r $! return $! off'
    return off'
    
rasterPicture
    :: Gloss.State -> Offscreen
    -> ScreenSize
    -> Picture
    -> IO RasteredPicture
rasterPicture state offscreen dim@(w,h) pic = do
    -- Save OpenGL state
    oldVP   <- GL.get GL.viewport
    
    -- Bind offscreen target
    GL.bindFramebuffer GL.Framebuffer $= (offFBO offscreen)
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
    
    -- Clear if needed (alpha = 0 for transparency)
    GL.clearColor $= GL.Color4 0 0 0 0
    GL.clear [GL.ColorBuffer]
    
    -- Set projection to match Gloss expectations
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.ortho (-fromIntegral w / 2) ( fromIntegral w / 2) (-fromIntegral h / 2) ( fromIntegral h / 2) (-1) 1
    
    GL.matrixMode $= GL.Modelview 0
    GL.loadIdentity
    
    -- Render Gloss picture
    renderPicture state 1 pic
    
    -- Read pixels (RGBA, bottom-up)
    let bytes = w * h * 4
    fptr <- mallocForeignPtrBytes bytes
    withForeignPtr fptr $ \ptr ->
        GL.readPixels (GL.Position 0 0) (GL.Size (fromIntegral w) (fromIntegral h)) (GL.PixelData GL.RGBA GL.UnsignedByte ptr)
    
    -- Restore GL state
    GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject
    GL.viewport $= oldVP
    
    -- Convert to Picture
    return (dim,BitmapFormat BottomToTop PxRGBA,fptr)

type RasteredPicture = (ScreenSize,BitmapFormat,ForeignPtr Word8)

fromRasteredPicture :: RasteredPicture -> Picture
fromRasteredPicture ((w,h),fmt,fptr) = bitmapOfForeignPtr w h fmt fptr True

-- | Checks if the pixel of a certain point (with (0,0) centered in the image) is transparent.
getPixelAlpha :: RasteredPicture -> Point -> IO Word8
getPixelAlpha pic@(dim,_,_) p = do
    alpha <- lookupAlpha pic (centerToTopLeft dim p)
    return $ alpha

centerToTopLeft :: ScreenSize -> Point -> (Int,Int)
centerToTopLeft (w,h) (xc, yc) =
  ( round $ xc + realToFrac w / 2
  , round $ realToFrac h / 2 - yc
  )

topLeftToCenter :: ScreenSize -> (Int,Int) -> Point
topLeftToCenter (w,h) (xt, yt) =
  ( realToFrac xt - realToFrac w / 2
  , realToFrac h / 2 - realToFrac yt
  )

-- coordinates (x,y) with (0,0) being the top-left corner.
lookupAlpha :: RasteredPicture -> (Int,Int) -> IO Word8
lookupAlpha pic@((w,h),fmt,fptr) (x,y) =
  withForeignPtr fptr $ \ptr -> do
    let off = pixelOffset pic x y + alphaOffset (pixelFormat fmt)
    peekByteOff ptr off

alphaOffset :: PixelFormat -> Int
alphaOffset PxRGBA = 3
alphaOffset PxABGR = 0

pixelStride :: Int
pixelStride = 4

pixelOffset :: RasteredPicture -> Int -> Int -> Int
pixelOffset (dim,fmt,fptr) x y =
  let (w, h)   = dim
      BitmapFormat ro pf = fmt
      ry       = rowIndex ro h y
      base     = (ry * w + x) * pixelStride
  in base

rowIndex :: RowOrder -> Int -> Int -> Int
rowIndex TopToBottom h y = y
rowIndex BottomToTop h y = h - 1 - y

