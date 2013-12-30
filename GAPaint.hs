-- |This module contains the Genetic Algorithm primitives

module GAPaint where

import Bitmap (Triangle(..), Vertex(..), Color(..),Bitmap2(..),writePNM2)
import Render (trianglesToBitmap)

import System.Random
import Control.Monad (replicateM)
import Data.Word (Word8)
import Data.List (maximumBy)
import qualified Data.ByteString as BS

---------------------------------------------------
-- GA Parameters

initialPop = 20 -- Size of the initial candidate pool

minTriangles = 1  -- Minimum number of triangles for initial candidate
maxTriangles = 10 -- Maximum number of triangles for initial candidate

genSize = 10 -- Size of the population to generate each iteration
tourSize = 3 -- Size of each tournament instance

pCrossover = 0.7 -- Probability of performing crossover between two candidates
pMutation = 0.1 -- Probability of mutating one candidate
---------------------------------------------------

-- |A candidate has a list of triangles
type Candidate = [Triangle]

-- |A candidate's Score is its fitness (the lower the better)
data Score = Score {
  candidate :: Candidate,
  fitness :: Double
  }

-- |Generate a random Integer between lo and hi, both included
randInt :: Int -> Int -> IO Int
randInt lo hi = getStdRandom (randomR (lo,hi))

-- |Generate a random Double between lo and hi, both included
randD :: Double -> Double -> IO Double
randD lo hi = getStdRandom (randomR (lo,hi))

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

-- |Generate one candidate with random triangles
generateCandidate :: (Int,Int) -> IO Candidate
generateCandidate (w,h) = do
  nTriangles <- randInt minTriangles maxTriangles
  replicateM nTriangles (randomTriangle (w,h))

-- |Generate initialPop # of candidates
generatePool :: (Int,Int) -> IO [Candidate]
generatePool (w,h) = replicateM initialPop (generateCandidate (w,h))

-- |Given a target picture and a candidate, return the score
score :: BS.ByteString -> (Int,Int)  -> Candidate -> IO Score
score target (w,h) candidate = do
  pixels <- trianglesToBitmap candidate (w,h)
  putStrLn $ "Fitness: " ++ (show (fitnessF target pixels))
  return $ Score candidate (fitnessF target pixels)

-- |The fitness function
fitnessF :: BS.ByteString -> BS.ByteString -> Double
fitnessF t c = total / (fromIntegral (BS.length t))
  where absum x y = if x > y then x - y else y - x
        total = sum $ map fromIntegral $ BS.zipWith absum t c

-- |Generate a list of n unique indices between 0 and max (inclusive), making
-- sure that they are not repeated
generateUniqueIndices :: Int -> Int -> IO [Int]
generateUniqueIndices n max = genUniq n []
  where genUniq 0 is = return is
        genUniq n is = do
          i <- randInt 0 max
          if (i `elem` is) then genUniq n is else genUniq (n-1) (i:is)

-- |Tournament selection: from a pool of n individuals, pick 'tourSize' of
-- them. Out of those, the one that survives is the one with the best fitness
-- score. Repeat until there are 'genSize' results
tournament :: [Score] -> Int -> Int -> IO [Score]
tournament scores genSize tourSize = tournament' scores genSize tourSize []
    where scoreCmp (Score _ f1) (Score _ f2) = compare f2 f1 -- look for min!
          tournament' sc 0 ts res = return res
          tournament' sc gs ts res = do
            indices <- generateUniqueIndices ts ((length sc)-1)
            let winner = maximumBy scoreCmp $ map (sc !!) indices
            tournament' sc (gs-1) ts (winner:res)


-- |Return the average fitness of a list of scores
avgFitness :: [Score] -> Double
avgFitness scores = (sum $ map fitness scores) / (fromIntegral (length scores))

--------------------------------
-- |Main function
--------------------------------
evolve :: Bitmap2 -> IO ()
evolve (Bitmap2 target dims) = do

  -- generate initial candidate pool
  pool <- generatePool dims

  putStrLn $ "Target dimensions: " ++ (show dims)
  putStrLn $ "Target lenght: " ++ (show (BS.length target))

  -- calculate the candidates' scores
  scores <- mapM (score target dims) pool

  bmp <- (trianglesToBitmap (head pool) dims)
  writePNM2 "triangels.pnm" bmp dims

  -- pick a set of candidates for the next iteration
  winners <- tournament scores genSize tourSize

  -- output the average fitness
  putStrLn $ "Avg fitness: " ++ (show $ avgFitness winners)
  putStrLn "Bye"
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


-----------------------
---- Can't use roulette for a minimization problem...
-- accumPick :: Double -> [(Double, Score)] -> Score
-- accumPick choice ss = accumPick' 0 ss
--   where accumPick' n ((p,s):ss) = if choice <= (n+p)
--                                   then s else accumPick' (n+p) ss

-- -- |Roulette: given an array of (candidate,fitness) pairs, choose a candidate
-- -- proportionally to its fitness. Choice must be a number between 0 and 1
-- -- t - x / t * (n-1)
-- roulette :: Double -> [Score] -> Score
-- roulette choice candidates = let
--   tot = sum $ map fitness candidates
--   len = fromIntegral(length candidates)
--   probs = map (\s@(Score _ f) -> ((tot - f) / (tot * (len-1)), s)) candidates
--   --sortedScores = sortBy cmpProb probs
--   --cmpProb (p1, _) (p2, _) = compare p1 p2
--   -- invScores = map (\s@(Score _ f) -> (totalFitness - f, s)) candidates
--   in
--    accumPick choice probs
