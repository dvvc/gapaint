-- |This module contains primitives to handle bitmaps and read them from image
-- files.

module Bitmap where

import Data.Array
import Data.Char (isSpace)
import Data.Word (Word8)

-- |A Color, with a red, green and blue values
data Color = Color {
  red :: Int, 
  green :: Int, 
  blue :: Int
  } deriving Show


-- |A Triangle vertex, with 2 coordinates
data Vertex = Vertex {
  x :: Int,
  y :: Int } deriving Show

-- |A Triangle
data Triangle = Triangle {
  vertices :: (Vertex, Vertex, Vertex),
  color :: Color,
  alpha :: Int
  } deriving Show


-- |A Bitmap is just an alias for a 2D array of pixels
type Bitmap = Array (Int, Int) Color 

showArray :: Bitmap -> String
showArray = show

-- |Returns the size of a Bitmap
getBitmapSize :: Bitmap -> (Int,Int)
getBitmapSize bitmap  = case bounds bitmap of
  ((0,0),(w,h)) -> (w+1,h+1)
  _ -> error "Invalid dimensions"


-- |Converts a pixel into a string
pixelToStr :: Color -> String
pixelToStr (Color r g b) = unlines $ map show [r,g,b]

-- |Converts pixels in a bitmap into their string representation
bitmapToStr :: Bitmap -> String
bitmapToStr = concatMap pixelToStr . elems

-- |Writes a Bitmap into a PNM file
writePNM :: String -> Bitmap -> IO ()
writePNM fname bitmap = let (w,h) = getBitmapSize bitmap
                        in writeFile fname ("P3\n# \n" ++ 
                                            show w ++ " " ++
                                            show h ++ "\n" ++
                                            "255\n" ++
                                            bitmapToStr bitmap)

writePNM2 :: String -> [Word8] -> (Int,Int) -> IO ()
writePNM2 fname pixels (w,h) = writeFile fname ("P3\n# \n" ++ 
                                                show w ++ " " ++
                                                show h ++ "\n" ++
                                                "255\n" ++
                                                serializePixels pixels)
  where serializePixels pixels = unlines $ map show pixels
    


-- |Reads dimensions from the first line
readPNMDim :: [String] -> (Int, Int)
readPNMDim dim = (w, h) where 
  [w,h] = map read $ words $ head dim

-- |Parses a list of strings in the form r\ng\nb\nr\n... to pixels
readPixels :: [String] -> [Color]
readPixels [] = []
readPixels pixelStrings = reverse $ readPixels' pixelStrings []
  where 
    readPixels' :: [String] -> [Color] -> [Color]
    readPixels' [] pixels = pixels
    readPixels' (r:g:b:pxs) accum = readPixels' pxs  
                                    (Color (read r) (read g) (read b) :accum)
                      
-- |Reads a PNM image into a Bitmap
readPNM :: String -> IO Bitmap
readPNM filename = do
  contents <- readFile filename
  let  noHeaders = drop 2 $ lines contents
       (w,h) = readPNMDim noHeaders
       pixels = readPixels (drop 2 noHeaders)
    in return $ listArray ((0,0),(w-1,h-1)) pixels
