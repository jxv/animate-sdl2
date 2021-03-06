{-# LANGUAGE DeriveAnyClass #-}
module Main where

import qualified SDL
import qualified Animate
import qualified Animate.SDL as AS

import Control.Monad (unless, when)
import Control.Concurrent (threadDelay)
import Data.StateVar (($=))
import SDL.Vect (V4(..), V2(..))
import SDL.FPS
import Paths_animate_sdl2_example (getDataFileName)

data DinoKey
  = DinoKey'Idle
  | DinoKey'Move
  | DinoKey'Kick
  | DinoKey'Hurt
  | DinoKey'Sneak
  deriving (Show, Eq, Ord, Bounded, Enum, Animate.KeyName)

detectSpacePressed :: SDL.EventPayload -> Bool
detectSpacePressed event = case event of
  SDL.KeyboardEvent SDL.KeyboardEventData{keyboardEventKeysym = SDL.Keysym{keysymKeycode = code}, keyboardEventKeyMotion = motion, keyboardEventRepeat = repeated} ->
    code == SDL.KeycodeSpace &&
    motion == SDL.Pressed &&
    not repeated
  _ -> False
  
fps :: Int
fps = 60

spf :: Float
spf = 1 / fromIntegral fps

loop
  :: SDL.Renderer
  -> Animate.SpriteSheet DinoKey SDL.Texture Float
  -> Animate.Position DinoKey Float
  -> IO ()
loop renderer ss@Animate.SpriteSheet{ssAnimations, ssImage} pos = do
  ticks <- startFrame
  -- clear screen
  SDL.rendererDrawColor renderer $= V4 0x33 0x33 0x33 0xff
  _ <- SDL.clear renderer
  -- input
  events <- map SDL.eventPayload <$> SDL.pollEvents
  let quit = elem SDL.QuitEvent events
  let toNextKey = any detectSpacePressed events
  -- animation
  let pos' = Animate.stepPosition ssAnimations pos spf
  let clip = Animate.currentLocation ssAnimations pos'
  AS.drawSprite renderer ss clip (V2 160 100)
  SDL.present renderer
  -- next key
  let pos'' = if toNextKey then Animate.initPosition (Animate.nextKey (Animate.pKey pos')) else pos'
  when toNextKey $ print $ Animate.keyName (Animate.pKey pos'')
  -- delay and loop
  endFrame fps ticks
  unless quit $ loop renderer ss pos''

main :: IO ()
main = do
  -- setup
  putStrLn "Press Space to iterate through animation keys"
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "Animate Example" SDL.defaultWindow { SDL.windowInitialSize = V2 320 180 }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  -- load sprites
  spriteSheet <- AS.loadSpriteSheetYamlWithPathFilter renderer getDataFileName "dino.yaml"
  -- run
  loop renderer spriteSheet (Animate.initPosition DinoKey'Idle)
  -- clean up
  SDL.destroyWindow window
  SDL.quit