module Main (main) where

import Bitmap (Triangle(..),
               Color(..),
               Vertex(..),
               Bitmap2(..),
               readPNM2,
               writePNM2)

import qualified Render (init, close)
import GAPaint (evolve)

-- triangles :: [Triangle]
-- triangles = [Triangle  (Vertex 0 0, Vertex 200 0 ,Vertex 100 100) (Color 1 0 0) 1]

-- main = do ts <- trianglesToBitmap triangles (400,400)
-- --          writePNM2 "triangle.pnm" ts (400,400)
--           return ()


--main = randomTriangle (400,400) >>= (putStrLn . show)

main = do
  wId <- Render.init (400,400)
  bmp <- readPNM2 "mona400.pnm"
  evolve bmp
  putStrLn "Done"
  Render.close wId
