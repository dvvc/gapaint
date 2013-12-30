-- |This module deals with writing the shapes to a framebuffer and retriving
-- the pixels back for comparison
-- Note that this code can probably be improved since 1) I am not sure OpenGL
-- is the best way to convert from shapes to pixel maps and 2) even if it is,
-- the logic of HOpenGL -> readPixels -> list of pixel values seems not very
-- efficient
{-# LANGUAGE BangPatterns #-}
module Render where

import Graphics.UI.GLUT hiding (Triangle)
import Graphics.Rendering.OpenGL hiding (Triangle)
import Graphics.Rendering.OpenGL.GL.VertexArrays
import Data.Array.Storable
import Data.Word (Word8)
import Data.List.Split (chunksOf)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
--import Data.ByteString.Internal (fromForeignPtr)
import Foreign.ForeignPtr (newForeignPtr_, touchForeignPtr)

import Foreign.Marshal.Array (peekArray)
import Foreign.Marshal.Alloc (allocaBytes)

import qualified Bitmap as B -- (Triangle(..), Color(..), red, green, blue, x, y)

-- Based on http://stackoverflow.com/a/5289424
groups :: Int -> BS.ByteString -> [BS.ByteString]
groups n = map (BS.take n) . takeWhile ((/=) BS.empty) . iterate (BS.drop n)

removeEveryNth :: Int -> BS.ByteString -> BS.ByteString
removeEveryNth n bs = BS.concat $ map (BS.take (n-1)) $ groups n bs

dropAlpha :: BS.ByteString -> BS.ByteString
dropAlpha = removeEveryNth 4

-- |One time initialization
init :: (Int,Int) -> IO Window
init (w,h) = do
  getArgsAndInitialize
  initialDisplayMode $= [SingleBuffered,RGBAMode]
  wId <- createWindow "Hello World"
  windowSize $= Size (fromIntegral w) (fromIntegral h)
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  ortho2D 0 (fromIntegral (w-1)) 0 (fromIntegral (w-1))
  reshapeCallback $= Just reshape
  return wId

close :: Window -> IO ()
close = destroyWindow

trianglesToBitmap :: [B.Triangle] -> (Int, Int) -> IO BS.ByteString
trianglesToBitmap ts (w,h) = do

  displayCallback $= (displayTriangles ts)
  mainLoopEvent -- force redraw
  pixels <- getpixels (w,h)
  return $ dropAlpha pixels

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing

buildTriangles :: [B.Triangle] -> [IO ()]
buildTriangles [] = []
buildTriangles ((B.Triangle (v1,v2,v3) c a):ts) =
  let color4f r g b a = color $ Color4 r g b (a :: GLfloat)
      vertex2f x y = vertex $ Vertex2 x (y :: GLfloat)
      vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)
      fi = fromIntegral
      nc x = (fi x) / 255 -- Normalize (0..255) -> (0..1)
  in [color4f (nc $ B.red c) (nc $ B.green c) (nc $ B.blue c) (nc a)
     ,vertex2f (fi (B.x v1)) (fi (B.y v1))
     ,vertex2f (fi (B.x v2)) (fi (B.y v2))
     ,vertex2f (fi (B.x v3)) (fi (B.y v3))] ++ buildTriangles ts


displayTriangles :: [B.Triangle] -> IO ()
displayTriangles ts = do
  clear [ColorBuffer]
  renderPrimitive Triangles $ sequence_ $
    buildTriangles ts --[B.Triangle (B.Vertex 0 0, B.Vertex 100 0, B.Vertex 100 100) (B.Color 255 255 255) 255]
  flush
  postRedisplay Nothing


preservingBufferBinding :: BufferTarget -> IO a -> IO a
preservingBufferBinding target action = do
  buffer <- get $ bindBuffer target
  result <- action
  bindBuffer target $= buffer
  return result


-- |Warning! This function returns the rows of the image flipped
getpixels :: (Int,Int) -> IO BS.ByteString
getpixels (w,h) = do

  let arraySize = (fromIntegral (w * h * 4))
  bs2 <- allocaBytes arraySize $ \ptr -> do
    --preservingBufferBinding PixelPackBuffer $ do
      --bindBuffer PixelPackBuffer $= Nothing
      --preservingClientAttrib [PixelStoreAttributes] $ do
      --  rowAlignment Pack $= 1
    readPixels (Position 0 0) (Size (fromIntegral w) (fromIntegral h)) $ PixelData RGBA UnsignedByte ptr


    bs <- BSI.create arraySize $ \d -> BSI.memcpy d ptr arraySize


--    a <- peekArray arraySize ptr

--    fptr <- newForeignPtr_ ptr
--    touchForeignPtr fptr

--    let bs = BSI.fromForeignPtr fptr 0 arraySize
    return bs

  return bs2
