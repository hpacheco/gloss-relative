module Graphics.Gloss.Relative.Internal.Cache where

import Graphics.Gloss.Relative.Internal.Window
import Graphics.Gloss.Relative.Internal.Dimension
import Graphics.Gloss.Relative.Internal.Data
import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map
import Data.IORef
import System.Mem.StableName
import Control.Monad.Trans

type CacheKey = (ScreenSize,String,Int)
type CacheVal = (WindowOutput,Int)
type CacheTable = IORef (Map CacheKey CacheVal)

newCacheTable :: IO CacheTable
newCacheTable = newIORef Map.empty

insertCacheTable :: Int -> CacheTable -> CacheKey -> WindowOutput -> IO ()
insertCacheTable i cache key val = do
    atomicModifyIORef' cache $ \m -> (Map.insert key (val,i) m, ())
    
lookupCacheTable :: Int -> CacheTable -> CacheKey -> IO (Maybe WindowOutput)
lookupCacheTable i cache key = do
    table <- readIORef cache
    case Map.lookup key table of
        Nothing -> return Nothing
        Just (v,_) -> do
            writeIORef cache $! Map.insert key (v,i) table
            return $ Just v

cachedRenderFrame :: Int -> CacheTable -> (String -> Frame -> Window IO ()) -> String -> Frame -> Window IO ()
cachedRenderFrame i cache render label f = do
    dim <- askDimension
    let screen = dimensionToScreenSize dim
    sn  <- liftIO $ makeStableName f
    let key = (screen,label,hashStableName sn)
    mb <- liftIO $ lookupCacheTable i cache key
    case mb of
      Just wo -> tellWindowOutput wo
      Nothing  -> do
          wo <- lift $ execWindow dim (render label f)
          liftIO $ insertCacheTable i cache key wo
          tellWindowOutput wo

evictOldCacheTable :: Int -> Int -> CacheTable -> IO ()
evictOldCacheTable frame maxAge cache = atomicModifyIORef' cache $ \m -> (Map.filter (\ce -> frame - snd ce <= maxAge) m,())
  
  
  