{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
module Main where

import           Graphics.RecordGL
import           Keyboard2D                (moveCamera)
import           Record
import           Record.Types
import           Window                    (UI (..), initGL)

import           Control.Applicative       ((<$>), (<*>))
import           Control.Lens
import           Data.Foldable             (foldMap, traverse_)
import           Graphics.GLUtil           (bufferIndices, drawIndexedTris,
                                            makeVAO, program, readTexture,
                                            simpleShaderProgram, texture2DWrap,
                                            withTextures2D, withVAO)
import           Graphics.GLUtil.Camera2D
import           Graphics.Rendering.OpenGL (BlendingFactor (..),
                                            BufferTarget (..),
                                            Capability (Enabled), Clamping (..),
                                            ClearBuffer (..), Color4 (..),
                                            GLfloat, GLint, Repetition (..),
                                            TextureFilter (..),
                                            TextureObject (..),
                                            TextureTarget2D (..), bindBuffer,
                                            blend, blendFunc, clear, clearColor,
                                            currentProgram, textureFilter, ($=))
import           Graphics.UI.GLFW          (Key (Key'Escape))
import           Linear                    (M33, V2 (..), _x)
import           System.FilePath           ((</>))

-- A record each drawing function will receive
type AppInfo = [r| {cam :: M33 GLfloat} |]

-- The ground levle for each column in our map. Heighs go up from 0 to 10
gameLevel :: [Int]
gameLevel =  [3,3,3,4,5,4,3,3,3,4,5,5,6,7,6,6,6,7,6,5,4,3,3]

-- Produce square vertices in normalized ([0,1]) coordinates for a tile
-- whose top is at the given height
tile :: Int -> [V2 GLfloat]
tile h = V2 <$> [0,0.2] <*> [h', h' - 0.2]
  where
    h' = fromIntegral h / 10

type Vertex = [r| {vertexCoord :: V2 GLfloat, texCoord :: V2 GLfloat} |]

pos :: (Field "vertexCoord" a a' v v', Functor f) => (v -> f v') -> a -> f a'
pos = [l|vertexCoord|]

tex :: (Field "texCoord" a a' v v', Functor f) => (v -> f v') -> a -> f a'
tex = [l|texCoord|]


-- Each element of the outer list is a list of the vertices that make up
-- a column. Push each successsive column farther along the X axis.
spaceColumns :: [[V2 GLfloat]] -> [[V2 GLfloat]]
spaceColumns = zipWith (map . (_x +~)) [0, 0.2..]


-- Compute a textured vertex recod for each input vertex
tileTex :: [[V2 GLfloat]] -> [Vertex]
tileTex = foldMap (zipWith (\t p -> [r| {vertexCoord = p, texCoord = t} |]) (cycle texes))
  where
    texes = V2 <$> [0,1] <*> [0,1]



-- Load the geometry data for all grass tiles into OpenGL.
grassTiles :: IO (BufferedVertices Vertex)
grassTiles = bufferVertices . tileTex . spaceColumns $ map tile gameLevel

-- Load the geometry data for all the dirt tiles into OpenGL.
dirtTiles :: IO (BufferedVertices Vertex)
dirtTiles = bufferVertices . tileTex . spaceColumns $ map col gameLevel
  where
    col :: Int -> [V2 GLfloat]
    col h = foldMap tile [h - 1, h - 2 .. 1]

-- Load a list of textures. Set each texture to use NN filtering.
loadTextures :: [FilePath] -> IO [TextureObject]
loadTextures = fmap (either error id . sequence) . mapM aux
  where aux f = do
          img <- readTexture f
          traverse_ (const texFilter) img
          return img
        texFilter = do
          textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
          texture2DWrap $= (Repeated, ClampToEdge)

-- Ground textures from: http://opengameart.org/content/platformer-tiles
-- Attributed to: "Kenney.nl" or "www.kenney.nl"
background :: IO (AppInfo -> IO ())
background = do
    [grass, dirt] <- loadTextures $ map ("assets" </>) ["ground.png", "ground_dirt.png"]
    s <- simpleShaderProgram ("shaders" </> "game2d.vert") ("shaders" </> "game2d.frag")
    putStrLn "Loaded shaders"
    setUniforms s (texSampler 0)
    grassVerts <- grassTiles
    eb <- bufferIndices inds
    grassVAO <- makeVAO $ do
      enableVertices' s grassVerts
      bindVertices grassVerts
      bindBuffer ElementArrayBuffer $= Just eb
    dirtVerts <- dirtTiles
    dirtVAO <- makeVAO $ do
      enableVertices' s dirtVerts
      bindVertices dirtVerts
      bindBuffer ElementArrayBuffer $= Just eb
    return $ \i -> do
      currentProgram $= Just (program s)
      setUniforms s i
      withVAO grassVAO . withTextures2D [grass] $
        drawIndexedTris numGrassTris
      withVAO dirtVAO . withTextures2D [dirt] $
        drawIndexedTris numDirtTris
  where
    texSampler i = [r| {tex = (i :: GLint)} |]
    inds = take (sum $ map (*6) gameLevel) $
           foldMap (flip map [0,1,2,2,1,3] . (+)) [0,4..]
    numGrassTris = fromIntegral $ 2 * length gameLevel
    numDirtTris = fromIntegral . sum $ map (*2) gameLevel

setup :: IO (AppInfo -> IO ())
setup = do
    clearColor $= Color4 0.812 0.957 0.969 1
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    background

-- The game loop makes use of a frame-tick action that provides an
-- updated 'UI' value and the drawing function returned from 'setup'.
loop :: IO UI -> IO ()
loop tick = setup >>= go camera2D
  where
    go :: Camera GLfloat -> (AppInfo -> IO ()) -> IO ()
    go c draw = do
      ui <- tick
      clear [ColorBuffer, DepthBuffer]
      let mCam = camMatrix c
          info = [r|{cam = mCam}|]
      draw info
      if keysPressed ui ^. contains Key'Escape
         then return ()
         else go (moveCamera ui c) draw

main :: IO ()
main = usage >> initGL "2D Platformer" 640 480 >>= loop

usage :: IO ()
usage = putStrLn "Arrow keys to translate, shift+arrow to rotate, ESC to exit."
