-- |This module deals with writing the shapes to a framebuffer and retriving 
-- the pixels back for comparison
-- Note that this code can probably be improved since 1) I am not sure OpenGL 
-- is the best way to convert from shapes to pixel maps and 2) even if it is, 
-- the logic of HOpenGL -> readPixels -> list of pixel values seems not very 
-- efficient 

module Render where

import Graphics.UI.GLUT hiding (Triangle)
import Graphics.Rendering.OpenGL hiding (Triangle)
import Graphics.Rendering.OpenGL.GL.VertexArrays
import Data.Array.Storable
import Data.Word (Word8)

import Foreign.Marshal.Array (peekArray)
import Foreign.Marshal.Alloc (allocaBytes)

import qualified Bitmap as B (Triangle(..), Color(..), red, green, blue, x, y)

-- |Performs two operations on a list of color components in the form 
-- r,g,b,a,r,g,b,a as returned by readPixels: 1) removes the alpha from each 
-- group of four and 2) rearranges the rows (originally [rn...r0] -> [r0...rn]
-- TODO: Inefficient
arrangePixels :: [a] -> (Int, Int) -> [a]
arrangePixels [] _ = []
arrangePixels pv (w,h) = (arrangePixels rest (w,h)) ++ (dropAlpha row)
  where rlen = w * 4
        row = (take rlen pv)
        rest = (drop rlen pv)
        dropAlpha [] = []
        dropAlpha v = (take 3 v) ++ (dropAlpha (drop 4 v))

--transposePixels :: [Word8] -> (Int,Int) -> [Word8]
-- transposePixels [] _ = []
-- transposePixels ps (w,h) = (transposePixels (drop (w*4) ps) (w,h)) ++ 
--                            (dropFourth (take (w*4) ps))

-- dropFourth [] = []
-- dropFourth s = (take 3 s) ++ (dropFourth (drop 4 s))

trianglesToBitmap :: [B.Triangle] -> (Int, Int) -> IO [Word8]
trianglesToBitmap ts (w,h) = do
  getArgsAndInitialize
  initialDisplayMode $= [SingleBuffered,RGBAMode]
  wId <- createWindow "Hello World"
  windowSize $= Size (fromIntegral w) (fromIntegral h)
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  displayCallback $= (displayTriangles ts)
  ortho2D (-1) (fromIntegral (w-1)) (fromIntegral (w-1)) (-1)
  reshapeCallback $= Just reshape

  mainLoopEvent  
  mainLoopEvent
  mainLoopEvent
  mainLoopEvent
  mainLoopEvent
  mainLoopEvent
  mainLoopEvent
  mainLoopEvent
  mainLoopEvent
  mainLoopEvent
  mainLoopEvent
  mainLoopEvent

  pixels <- getpixels (w,h)
  destroyWindow wId
  return $ arrangePixels pixels (w,h)


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
  in [color4f (fi (B.red c)) (fi (B.green c)) (fi (B.blue c)) (fi a)
     ,vertex2f (fi (B.x v1)) (fi (B.y v1))
     ,vertex2f (fi (B.x v2)) (fi (B.y v2))
     ,vertex2f (fi (B.x v3)) (fi (B.y v3))] ++ buildTriangles ts


-- drawTriangles 
--   renderPrimitive Points $ do
--     color4f 1 0.5 0 1
--     mapM (\(x,y) -> vertex2f (fromIntegral x) (fromIntegral y)) pixels


displayTriangles :: [B.Triangle] -> IO () 
displayTriangles ts = do
  clear [ColorBuffer]
  renderPrimitive Triangles $ sequence_ $ buildTriangles ts
  flush
  postRedisplay Nothing
  
  
type PixelArray = StorableArray Int Int

getpixels :: (Int,Int) -> IO [Word8]
getpixels (w,h) = do
  --pixelArray <- newArray (0, 9) 0 :: IO PixelArray
  let arraySize = (fromIntegral (w * h * 4))
  allocaBytes arraySize $ \ptr -> do
    readPixels (Position 0 0) (Size (fromIntegral w) (fromIntegral h)) 
      $ PixelData RGBA UnsignedByte ptr
    peekArray arraySize ptr