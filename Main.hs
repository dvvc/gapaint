module Main (main) where

import Bitmap (Triangle(..),
               Color(..),
               Vertex(..),
               Bitmap2(..),
               readPNM2,
               writePNM2)

import qualified Render (init, close)
import GAPaint (evolve)
import System.Environment

import qualified Data.ByteString as BS

-- triangles :: [Triangle]
-- triangles = [Triangle  (Vertex 0 0, Vertex 200 0 ,Vertex 100 100) (Color 1 0 0) 1]

-- main = do ts <- trianglesToBitmap triangles (400,400)
-- --          writePNM2 "triangle.pnm" ts (400,400)
--           return ()


--main = randomTriangle (400,400) >>= (putStrLn . show)

main = do
  args <- getArgs
  wId <- Render.init (400,400)
  bmp <- readPNM2 $ head args --"mona400.pnm"

  putStrLn $ (show (BS.take 100 (pixels bmp)))

  cs <- evolve 100 bmp
  putStrLn "Done"
  Render.close wId
