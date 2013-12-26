module Bitmap where

import Data.Array
import Data.Char (isSpace)

data Pixel = Pixel {
  red :: Int, 
  green :: Int, 
  blue :: Int
  } deriving Show


type Bitmap = Array (Int, Int) Pixel 

showArray :: Bitmap -> String
showArray bm = show bm

evolve :: IO ()
evolve = do contents <- readPNM "mona.pnm"
            writePNM "mona2.pnm" contents
         
            
-- Returns the size of a Bitmap
getBitmapSize :: Bitmap -> (Int,Int)
getBitmapSize bitmap  = case bounds bitmap of
  ((0,0),(w,h)) -> (w+1,h+1)
  _ -> error "Invalid dimensions"


-- Convert a pixel into a string
pixelToStr :: Pixel -> String
pixelToStr (Pixel r g b) = unlines $ map show [r,g,b]

-- Convert pixels in a bitmap into their string representation
bitmapToStr :: Bitmap -> String
bitmapToStr = concat . (map pixelToStr) . elems

-- Write a Bitmap into a PNM file
writePNM :: String -> Bitmap -> IO ()
writePNM fname bitmap = let (w,h) = getBitmapSize bitmap
                        in writeFile fname ("P3\n# \n" ++ 
                                            (show w) ++ " " ++ 
                                            (show h) ++ "\n" ++
                                            "255\n" ++
                                            (bitmapToStr bitmap))

-- Read dimensions from the first line
readPNMDim :: [String] -> (Int, Int)
readPNMDim dim = (w, h) where 
  [w,h] = map read $ words $ dim !! 0

-- Parse a list of strings in the form r\ng\nb\nr\n... to pixels
readPixels :: [String] -> [Pixel]
readPixels [] = []
readPixels pixelStrings = reverse $ readPixels' pixelStrings []
  where 
    readPixels' :: [String] -> [Pixel] -> [Pixel]
    readPixels' [] pixels = pixels
    readPixels' (r:g:b:pxs) accum = readPixels' pxs  
                                    ((Pixel (read r) (read g) (read b)):accum)
                      
-- Reads a PNM image into a Bitmap
readPNM :: String -> IO Bitmap
readPNM filename = do
  contents <- readFile filename
  let  noHeaders = drop 2 $ lines contents
       (w,h) = readPNMDim noHeaders
       pixels = readPixels (drop 2 noHeaders)
    in return $ listArray ((0,0),(w-1,h-1)) pixels