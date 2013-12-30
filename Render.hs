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

-- |Performs two operations on a list of color components in the form
-- r,g,b,a,r,g,b,a as returned by readPixels: 1) removes the alpha from each
-- group of four and 2) rearranges the rows (originally [rn...r0] -> [r0...rn]
-- TODO: Inefficient
-- arrangePixels :: [a] -> (Int, Int) -> [a]
-- arrangePixels [] _ = []
-- arrangePixels pv (w,h) = (arrangePixels rest (w,h)) ++ (dropAlpha row)
--   where rlen = w * 4
--         row = (take rlen pv)
--         rest = (drop rlen pv)
--         dropAlpha [] = []
--         dropAlpha v = (take 3 v) ++ (dropAlpha (drop 4 v))



-- Based on http://stackoverflow.com/a/5289424
groups :: Int -> BS.ByteString -> [BS.ByteString]
groups n = map (BS.take n) . takeWhile ((/=) BS.empty) . iterate (BS.drop n)

removeEveryNth :: Int -> BS.ByteString -> BS.ByteString
removeEveryNth n bs = BS.concat $ map (BS.take (n-1)) $ groups n bs

dropAlpha :: BS.ByteString -> BS.ByteString
dropAlpha = removeEveryNth 4


--removeEveryNth 4
--dropAlpha bs = if bs == BS.empty
--               then BS.empty
--               else BS.append (BS.take 3 bs) (dropAlpha (BS.drop 4 bs))
-- dropAlpha bs = dropAlpha' bs BS.empty
--   where dropAlpha' bs !acc = if bs == BS.empty
--                             then acc
--                             else dropAlpha' (BS.drop 4 bs) (BS.append (BS.take 3 bs) acc)



-- reverseRows :: Int -> ByteString -> ByteString
-- reverseRows w pv = concat $ reverse $ chunksOf w pv

-- arrangePixels :: ByteString -> (Int, Int) -> ByteString
-- arrangePixels pv (w,h) = reverseRows (w*3) $ dropAlpha pv


--transposePixels :: [Word8] -> (Int,Int) -> [Word8]
-- transposePixels [] _ = []
-- transposePixels ps (w,h) = (transposePixels (drop (w*4) ps) (w,h)) ++
--                            (dropFourth (take (w*4) ps))

-- dropFourth [] = []
-- dropFourth s = (take 3 s) ++ (dropFourth (drop 4 s))

-- |One time initialization
init :: (Int,Int) -> IO Window
init (w,h) = do
  getArgsAndInitialize
  initialDisplayMode $= [SingleBuffered,RGBAMode]
  wId <- createWindow "Hello World"
  windowSize $= Size (fromIntegral w) (fromIntegral h)
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  ortho2D (-1) (fromIntegral (w-1)) (fromIntegral (w-1)) (-1)
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
  --pixels <- getpixels (w,h)
  --putStrLn "after getpixels"
  --putStrLn $ "len: " ++ (show (BS.length pixels))
  --putStrLn $ (show (BS.take 100 pixels))
  --let noAlpha = dropAlpha pixels
  --putStrLn $ "noalpha: " ++ (show (BS.length noAlpha))
  --return noAlpha


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


-- drawTriangles
--   renderPrimitive Points $ do
--     color4f 1 0.5 0 1
--     mapM (\(x,y) -> vertex2f (fromIntegral x) (fromIntegral y)) pixels


displayTriangles :: [B.Triangle] -> IO ()
displayTriangles ts = do
  clear [ColorBuffer]
  renderPrimitive Triangles $ sequence_ $
    buildTriangles [B.Triangle (B.Vertex 0 399,B.Vertex 10 399, B.Vertex 5 396) (B.Color 100 100 100) 200]
  flush
  postRedisplay Nothing


type PixelArray = StorableArray Int Int


preservingBufferBinding :: BufferTarget -> IO a -> IO a
preservingBufferBinding target action = do
  buffer <- get $ bindBuffer target
  result <- action
  bindBuffer target $= buffer
  return result


getpixels :: (Int,Int) -> IO BS.ByteString
getpixels (w,h) = do
  --pixelArray <- newArray (0, 9) 0 :: IO PixelArray
  let arraySize = (fromIntegral (w * h * 4))
  bs2 <- allocaBytes arraySize $ \ptr -> do
    --preservingBufferBinding PixelPackBuffer $ do
      --bindBuffer PixelPackBuffer $= Nothing
      --preservingClientAttrib [PixelStoreAttributes] $ do
      --  rowAlignment Pack $= 1
    readPixels (Position 0 0) (Size (fromIntegral w) (fromIntegral h)) $ PixelData RGBA UnsignedByte ptr


    bs <- BSI.create arraySize $ \d -> BSI.memcpy d ptr arraySize


    a <- peekArray arraySize ptr
    putStrLn $ "List: " ++ (show (take 800 a))

--    fptr <- newForeignPtr_ ptr
--    touchForeignPtr fptr
    putStrLn "return"
--    let bs = fromForeignPtr fptr 0 arraySize
    putStrLn $ "len fptr: " ++ (show (BS.length bs))
    putStrLn $ (show (BS.take 100 bs))
    return bs

  putStrLn "Returned"
  putStrLn $ (show (BS.length bs2))
  putStrLn $ (show (BS.take 100 bs2))
  return bs2
