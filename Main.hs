module Main (main) where

import Bitmap (Triangle(..), Color(..), Vertex(..), writePNM2)
import Render (trianglesToBitmap)
import GAPaint

-- triangles :: [Triangle]
-- triangles = [Triangle  (Vertex 0 0, Vertex 200 0 ,Vertex 100 100) (Color 1 0 0) 1]

-- main = do ts <- trianglesToBitmap triangles (400,400)
-- --          writePNM2 "triangle.pnm" ts (400,400)
--           return ()


main = randomTriangle (400,400) >>= (putStrLn . show)