module Animate.SDL
  ( loadTexture
  , loadSpriteSheetJson
  , loadSpriteSheetYaml
  , loadSpriteSheetJsonWithPathFilter
  , loadSpriteSheetYamlWithPathFilter
  , unloadSpriteSheet
  , drawSprite
  , drawSpriteWithScalar
  , rectFromClip
  , offsetFromClip
  ) where

import qualified SDL
import qualified SDL.Raw.Video as Raw
import qualified SDL.Internal.Numbered as Numbered
import qualified SDL.Image as Image
import Foreign.C.Types
import Animate
import Data.Aeson (FromJSON)
import Data.Maybe (fromMaybe)
import SDL.Vect (V4(..), V2(..))

-- | Produce a new 'SDL.Surface' based on an existing one, but
-- optimized for blitting to the specified 'SDL.PixelFormat'.
convertSurface :: SDL.Surface -> SDL.PixelFormat -> IO SDL.Surface
convertSurface (SDL.Surface s _) pixFmt = do
  fmt <- Raw.allocFormat (Numbered.toNumber pixFmt)
  surface <- SDL.Surface <$> Raw.convertSurface s fmt 0 <*> pure Nothing
  surface <$ Raw.freeFormat fmt

loadSurface :: FilePath -> Maybe Color -> IO SDL.Surface
loadSurface path alpha = do
  surface0 <- Image.load path
  surface <- convertSurface surface0 SDL.RGBA8888
  SDL.freeSurface surface0
  case alpha of
    Just (r,g,b) -> SDL.surfaceColorKey surface SDL.$= (Just $ V4 r g b 0x00)
    Nothing -> return ()
  return surface

loadTexture :: SDL.Renderer -> FilePath -> Maybe Color -> IO SDL.Texture
loadTexture ren filePath color = do
  s <- loadSurface filePath color
  t <- SDL.createTextureFromSurface ren s
  SDL.freeSurface s
  return t

loadSpriteSheetJson
  :: (KeyName key, Ord key, Bounded key, Enum key, FromJSON delay)
  => SDL.Renderer
  -> FilePath -- ^ Path of the sprite sheet info JSON file
  -> IO (SpriteSheet key SDL.Texture delay)
loadSpriteSheetJson ren = readSpriteSheetJSON (loadTexture ren)

loadSpriteSheetJsonWithPathFilter
  :: (KeyName key, Ord key, Bounded key, Enum key, FromJSON delay)
  => SDL.Renderer
  -> (FilePath -> IO FilePath) -- ^ Lookup image location with `getDataFileName` from Paths
  -> FilePath -- ^ Path of the sprite sheet info JSON file
  -> IO (SpriteSheet key SDL.Texture delay)
loadSpriteSheetJsonWithPathFilter ren getDataFileName = readSpriteSheetJSON $ \fp c -> do
  fp' <- getDataFileName fp
  loadTexture ren fp' c

loadSpriteSheetYaml
  :: (KeyName key, Ord key, Bounded key, Enum key, FromJSON delay)
  => SDL.Renderer
  -> FilePath -- ^ Path of the sprite sheet info YAML file
  -> IO (SpriteSheet key SDL.Texture delay)
loadSpriteSheetYaml ren = readSpriteSheetYAML (loadTexture ren)

loadSpriteSheetYamlWithPathFilter
  :: (KeyName key, Ord key, Bounded key, Enum key, FromJSON delay)
  => SDL.Renderer
  -> (FilePath -> IO FilePath) -- ^ Lookup image location with `getDataFileName` from Paths
  -> FilePath -- ^ Path of the sprite sheet info YAML file
  -> IO (SpriteSheet key SDL.Texture delay)
loadSpriteSheetYamlWithPathFilter ren getDataFileName = readSpriteSheetYAML $ \fp c -> do
  fp' <- getDataFileName fp
  loadTexture ren fp' c

unloadSpriteSheet :: SpriteSheet key SDL.Texture delay -> IO ()
unloadSpriteSheet ss = SDL.destroyTexture $ ssImage ss

drawSprite :: SDL.Renderer -> SpriteSheet key SDL.Texture delay -> SpriteClip key -> V2 Int -> IO ()
drawSprite ren SpriteSheet{ssImage=sheet} clip loc = do
  let clip'@(SDL.Rectangle _ dim) = rectFromClip clip
  let offset = offsetFromClip clip
  let loc' = (+) <$> offset <*> fmap fromIntegral loc
  SDL.copy ren sheet (Just clip') (Just $ SDL.Rectangle (SDL.P loc') dim)

drawSpriteWithScalar :: SDL.Renderer -> V2 Float -> SpriteSheet key SDL.Texture delay -> SpriteClip key -> V2 Int -> IO ()
drawSpriteWithScalar ren scalar SpriteSheet{ssImage=sheet} clip loc = do
  let clip'@(SDL.Rectangle _ dim) = rectFromClip clip
  let offset = offsetFromClip clip
  let loc' =
        (+)
        <$> fmap round ((*) <$> fmap fromIntegral offset <*> scalar)
        <*> fmap fromIntegral loc
  SDL.rendererScale ren SDL.$= fmap CFloat scalar
  SDL.copy ren sheet (Just clip') (Just $ SDL.Rectangle (SDL.P loc') dim)
  SDL.rendererScale ren SDL.$= (V2 1 1)

rectFromClip :: SpriteClip key -> SDL.Rectangle CInt
rectFromClip SpriteClip{scX,scY,scW,scH} = SDL.Rectangle (SDL.P (V2 (num scX) (num scY))) (V2 (num scW) (num scH))
  where
    num = fromIntegral

offsetFromClip :: SpriteClip key -> V2 CInt
offsetFromClip SpriteClip{scOffset} = fromMaybe
  (V2 0 0)
  ((\(x,y) -> fromIntegral <$> V2 (-x) (-y)) <$> scOffset)