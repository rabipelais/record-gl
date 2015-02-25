{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
module Main where

import           Control.Applicative       ((<$>), (<*>))
import           Control.Lens
import           Data.Foldable             (foldMap, traverse_)
import           Graphics.GLUtil           (readTexture, texture2DWrap)
import           Graphics.RecordGL
import           Graphics.Rendering.OpenGL (BlendingFactor (..),
                                            Capability (Enabled), Clamping (..),
                                            Color4 (..), GLfloat,
                                            Repetition (..), TextureFilter (..),
                                            TextureObject (..),
                                            TextureTarget2D (..), blend,
                                            blendFunc, clearColor,
                                            textureFilter, ($=))
import           Linear                    (M33, V2 (..), _x)
import           Record
import           Record.Types
import           System.FilePath           ((</>))
import           Window                    (UI (..), initGL)

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

pos :: (FieldLens "vertexCoord" a a' v v', Functor f) => (v -> f v') -> a -> f a'
pos = [l|vertexCoord|]

tex :: (FieldLens "texCoord" a a' v v', Functor f) => (v -> f v') -> a -> f a'
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

loadTextures :: [FilePath] -> IO [TextureObject]
loadTextures = fmap (either error id . sequence) . mapM aux
  where aux f = do
          img <- readTexture f
          traverse_ (const texFilter) img
          return img
        texFilter = do
          textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
          texture2DWrap $= (Repeated, ClampToEdge)

background :: IO (AppInfo -> IO ())
background = do
    [grass, dirt] <- loadTextures $ map ("art" </>) ["ground.png", "ground_dirt.png"]
    -- s <- simpleShaderProgram ("etc" </> "game2d.vert") ("etc" </> "game2d.frag")
    -- putStrLn "Loaded shaders"
    -- setUniforms s (texSampler =: 0)
    -- grassVerts <- grassTiles
    -- eb <- bufferIndices ind
    undefined

setup :: IO (AppInfo -> IO ())
setup = do
    clearColor $= Color4 0.812 0.957 0.969 1
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    background

loop :: IO UI -> IO ()
loop = undefined

main :: IO ()
main = usage >> initGL "2D Platformer" 640 480 >>= loop

usage :: IO ()
usage = putStrLn "Arrow keys to translate, shift+arrow to rotate, ESC to exit."
