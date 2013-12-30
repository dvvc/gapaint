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

initialPop = 100 -- Size of the initial candidate pool

minTriangles = 1  -- Minimum number of triangles for initial candidate
maxTriangles = 10 -- Maximum number of triangles for initial candidate

genSize = 40 -- Size of the population to generate each iteration
tourSize = 3 -- Size of each tournament instance

pCrossover = 0.7 -- Probability of performing crossover between two candidates
pMutation = 0.1 -- Probability of mutating one candidate
pMutateAdd = 0.5 -- Prob of adding a triangle during mutation, (1-p) of removing
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
--  putStrLn $ "Fitness: " ++ (show (fitnessF target pixels))
--  putStrLn $ "Target: " ++ (show (take 100 (BS.unpack target)))
--  putStrLn $ "Candidate: " ++ (show (take 100 (BS.unpack pixels)))
--  putStrLn $ "Values: " ++ (show (take 100 (BS.zipWith absub target pixels)))
  return $ Score candidate (fitnessF target pixels)

absub x y = if x > y then x - y else y - x

-- |The fitness function
fitnessF :: BS.ByteString -> BS.ByteString -> Double
fitnessF t c = total / (fromIntegral (BS.length t))
  where absub x y = if x > y then x - y else y - x
        total = sum $ map fromIntegral $ BS.zipWith absub t c

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

-- |Cross two candidates at two points
-- TODO: Can be generalized
cross :: Int -> Int -> [a] -> [a] -> [[a]]
cross cp1 cp2 c1 c2 = let c1h1 = take cp1 c1
                          c2h1 = take cp2 c2
                          c1h2 = drop cp1 c1
                          c2h2 = drop cp2 c2
                      in [c1h1 ++ c2h2, c2h1 ++ c1h2]

randomCross :: [a] -> [a] -> IO [[a]]
randomCross c1 c2 = do
  cp1 <- randInt 1 ((length c1) - 1)
  cp2 <- randInt 1 ((length c2) - 1)
  return $ cross cp1 cp2 c1 c2

-- |For every pair of candidates, either cross with a probability of 'pc', or
-- return unmodified
crossover :: Double -> [[a]] -> IO [[a]]
crossover pc cs = crossover' cs []
  where crossover' [] acc = return acc
        crossover' (c1:c2:cs) acc = do
          choice <- randD 0 1
          if choice < pc
            then do
                 [c1',c2'] <- randomCross c1 c2
                 crossover' cs (c1':c2':acc)
            else crossover' cs (c1:c2:acc)


randomAdd :: (Int,Int) -> Candidate -> IO Candidate
randomAdd dims c = do
  i <- randInt 0 (length c)
  t <- randomTriangle dims
  return $ (take i c) ++ [t] ++ (drop i c)

randomDel :: Candidate -> IO Candidate
randomDel c = do
  i <- randInt 0 ((length c) - 1)
  return $ (take i c) ++ (drop (i+1) c)

randomMutation :: (Int,Int) -> Double -> Candidate -> IO Candidate
randomMutation dims pa c = do
  padd <- randD 0 1
  -- Always add when length is 1
  if (length c) == 1 || padd < pa
    then randomAdd dims c
    else randomDel c

-- |For each candidate, mutate with a probability of 'pm', or return
-- unmodified. Mutation either adds or removes a random triangle
mutate :: (Int,Int) -> Double -> [Candidate] -> IO [Candidate]
mutate dims pm cs = mutate' cs []
  where mutate' [] acc = return acc
        mutate' (c:cs) acc = do
          choice <- randD 0 1
          if choice < pm
             then do
                  c' <- randomMutation dims pMutateAdd c
                  mutate' cs (c':acc)
            else mutate' cs (c:acc)


--------------------------------
-- |Main function
--------------------------------
evolve :: Int -> Bitmap2 -> IO [Candidate]
evolve n target@(Bitmap2 tpixels dims) = do

  -- generate initial candidate pool
  pool <- generatePool dims

  putStrLn $ "Target dimensions: " ++ (show dims)
  putStrLn $ "Target lenght: " ++ (show (BS.length tpixels))

  evolveTimes n pool target


evolveTimes :: Int -> [Candidate] -> Bitmap2 -> IO [Candidate]
evolveTimes 0 pool target = return pool
evolveTimes x pool target@(Bitmap2 tpixels dims) = do

  -- calculate the candidates' scores
  scores <- mapM (score tpixels dims) pool

  putStrLn $ "Iteration " ++ (show x) ++ ": " ++ (show (avgFitness scores))
    ++ "(" ++ (show $ length scores) ++ ")"


  -- TODO: get the best
  bmp <- (trianglesToBitmap (candidate (scores !! 2)) dims)
  writePNM2 ("candidate_" ++ (show x) ++ ".pnm") bmp dims

  newPool <- evolveIteration scores target
  -- output the average fitness
  -- write the top candidate

  evolveTimes (x-1) newPool target



-- |Run one evolution iteration. Given a pool of candidates and a target bitmap,
-- generate a new set of candidates
evolveIteration :: [Score] -> Bitmap2 -> IO [Candidate]
evolveIteration pool (Bitmap2 tpixels dims) = do

  -- pick a set of candidates for the next iteration
  winners <- tournament pool genSize tourSize

  crossed <- crossover pCrossover $ map candidate winners

  mutated <- mutate dims pMutation crossed

  return mutated

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
