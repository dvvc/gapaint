-- |This module contains the Genetic Algorithm primitives

module GAPaint where

import Bitmap (Triangle(..), Vertex(..), Color(..))

import System.Random

-- evolve :: IO ()
-- evolve = do contents <- readPNM "mona.pnm"
--            writePNM "mona2.pnm" contents
         

-- |A Pool is a list of candidates
-- type Pool = [Candidate]
  


-- |Generate a random Integer between lo and hi, both included
randInt :: Int -> Int -> IO Int
randInt lo hi = getStdRandom (randomR (lo,hi))

-- |Generate a random alpha value between 0 and 255
randomAlpha :: IO Int
randomAlpha = randInt 0 255

-- |Generate a random color
randomColor :: IO Color
randomColor = do
  r <- randInt 0 255
  g <- randInt 0 255
  b <- randInt 0 255
  return $ Color r g b

-- |Generate a random Vertex
randomVertex :: (Int, Int) -> IO Vertex
randomVertex (w,h) = do
  x <- randInt 0 w
  y <- randInt 0 h
  return $ Vertex x y

-- |Generate a random triangle
randomTriangle :: (Int, Int) -> IO Triangle
randomTriangle (w,h) = do
  let maxW = w - 1
      maxH = h - 1
  v1 <- randomVertex (maxW,maxH)
  v2 <- randomVertex (maxW,maxH)
  v3 <- randomVertex (maxW,maxH)
  c <- randomColor
  a <- randomAlpha
  return $ Triangle (v1,v2,v3) c a


-- generate initial pool of size s
-- -- each candidate has a # of triangles between n m

-- crossover phase (s times):
-- -- select 2 parents based on highest fitness
-- -- if random(pcross) 
-- -- -- crossover to obtain 2 children
-- -- else:
-- -- -- clone

-- mutation phase (s times):
-- -- if random(pmut)
-- -- -- mutate individual

-- generation:
-- -- select # of triangles (t) between n, m
-- -- for 1..t, generate 3 vertices btw. 0..w,0..h
-- -- assign random r, g, b, a values

-- crossover:
-- -- find split points in p1, p2
-- -- exchange genes at those points

-- mutation:
-- -- if random(padd) -> add a new triangle
-- -- else: remove a triangle

-- fitness:
-- -- given a target image with pixels p@(r,g,b)... and a candidate with 
-- -- p2@(r2,g2,b2), fitness is = wsim * abs(p-p2) + wsiz * # triangles, where 
-- -- p-p2 = (r-r2+g-g2+b-b2)