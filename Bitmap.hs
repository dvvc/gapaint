-- |This module contains primitives to handle bitmaps and read them from image
-- files.

module Bitmap where

import Data.Array
import Data.Char (isSpace)
import Data.Word (Word8)
import Data.List (intersperse)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

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

data Bitmap2 = Bitmap2 {
  pixels :: BS.ByteString,
  dims :: (Int,Int)
}

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

writePNM2 :: String -> BS.ByteString -> (Int,Int) -> IO ()
writePNM2 fname pixels (w,h) = writeFile fname ("P3\n# \n" ++
                                                show w ++ " " ++
                                                show h ++ "\n" ++
                                                "255\n" ++
                                                serializePixels pixels)
  where serializePixels pixels = intersperse '\n' $ BSC.unpack pixels



-- |Reads dimensions from the first line
readPNMDim :: [String] -> (Int, Int)
readPNMDim dim = (w, h) where
  [w,h] = map read $ words $ head dim

-- |Parses a list of strings in the form [r,g,b,r,...] to pixels
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

-- |Reads dimensions from the first line
readPNMDim2 :: String -> (Int, Int)
readPNMDim2 dim = (w, h) where
  [w,h] = map read $ words dim


dropLines n xs = (iterate (BS.tail . BSC.dropWhile (/= '\n')) xs) !! n

-- |Reads a PNM image into a Bitmap2
readPNM2 :: String -> IO Bitmap2
readPNM2 filename = do
  putStrLn "Enter readPNM2"
  contents <- BS.readFile filename
  putStrLn $ "read " ++ (show (length $ BSC.lines contents))
  let noHeaders = dropLines 2 contents --BS.concat $ drop 2 $ BSC.lines contents
  let (w,h) = readPNMDim2 $ BSC.unpack $ head $ BSC.lines noHeaders
  putStrLn $ "dimensions: " ++ (show w) ++ " " ++ (show h)
  return (Bitmap2 (BS.drop 2 noHeaders) (w,h))
