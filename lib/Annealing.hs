{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-| Haskell implementation of simulated annealing
-}
module Annealing(
  AnnealingState(..),
  AssessedCandidate(..),
  annealing,
  -- Testing
  example
  ) where

import Control.Concurrent (forkIO)
import qualified Control.Concurrent.STM as STM
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.List as List
import GHC.Generics (Generic)
import Streaming
import qualified Streaming.Prelude as S
import System.Random.MWC.Probability (Gen)
import qualified System.Random.MWC.Probability as P

{-| A candidate solution with its fitness
-}
data AssessedCandidate s =
  AssessedCandidate
    { acCandidate :: !s
    , acFitness   :: !Double
    }
    deriving stock (Show, Generic)
    deriving anyclass (NFData)

{-| Annealing state
-}
data AnnealingState s =
  AnnealingState
    { asTime             :: !Int
    , asBestCandidate    :: !s
    , asCurrentCandidate :: !s
    , asLastProbability  :: !(Maybe Double)
    , asLastTemperature  :: !Double
    } deriving Show

initialState :: s -> AnnealingState s
initialState s = AnnealingState 1 s s Nothing 0

{-| The simulated annealing algorithm. It generates an infinite stream of
  'AnnealingState' values. Use @S.head_ $ S.dropWhile (not . accept)@ get
  the first solution that meets the acceptance criterion @accept@.
-}
annealing ::
  forall s.
  NFData s
  => Gen (PrimState IO)
  -> (Int -> Double) -- ^ Temperature
  -> (s -> Double) -- ^ Fitness function. 0 = perfect
  -> s -- ^ Initial state
  -> (s -> IO s) -- ^ Neighbour selection
  -> Stream (Of (AnnealingState (AssessedCandidate s))) IO () -- ^ Successive approximations
annealing gen temp fitness s neighbours = flip S.unfoldr (initialState (AssessedCandidate s (fitness s))) $ \AnnealingState{asTime, asBestCandidate, asCurrentCandidate, asLastProbability} -> do
  let currentTemp = temp asTime
      currentFit = acFitness asCurrentCandidate
      assess c = AssessedCandidate c (fitness c)

  let mkResultVar = do
          tv <- STM.atomically STM.newEmptyTMVar
          forkIO (neighbours (acCandidate asCurrentCandidate) >>= evaluate . force . assess >>= STM.atomically . STM.putTMVar tv)
          pure tv

      bestNeighbour :: IO (AssessedCandidate s)
      bestNeighbour = do
        tvars <- sequence (fmap (const mkResultVar) [1..8])
        list <- traverse (STM.atomically . STM.readTMVar) tvars
        return $ head $ List.sortOn acFitness list

      accept :: AssessedCandidate s -> IO (Maybe (AssessedCandidate s), Maybe Double)
      accept AssessedCandidate{acCandidate=y, acFitness=newFit} = do
        if newFit <= currentFit
          then return $ (Just (AssessedCandidate y newFit), Nothing)
          else do
            let r    = negate ((newFit - currentFit) / currentTemp)
                rate = exp r
            shouldAccept <- P.sample (P.bernoulli rate) gen
            if shouldAccept
              then return (Just (AssessedCandidate y newFit), Just rate)
              else return (Nothing, Just rate)

  (newCandidate, newProb) <- bestNeighbour >>= accept
  let newTime = succ asTime
      newProb' = asLastProbability <|> newProb
      newState = case newCandidate of
        Nothing -> AnnealingState newTime asBestCandidate asCurrentCandidate newProb' currentTemp
        Just ac@AssessedCandidate{acFitness=newFit} | newFit < acFitness asBestCandidate -> AnnealingState newTime ac              ac newProb' currentTemp
                                                    | otherwise                          -> AnnealingState newTime asBestCandidate ac newProb' currentTemp

  return (Right (newState, newState))

-- https://de.wikipedia.org/wiki/Simulated_Annealing

example :: IO ()
example = do
  gen <- P.createSystemRandom
  putStrLn "Sampling..."
  let stream =
        annealing
          gen
          (\i -> 1 + (1 / fromIntegral i))
          (\s -> abs (150 - s)) -- fitness = deviation from actual mean
          (0 :: Double)
          (\_ -> P.sample (P.normal 150 15) gen) -- try to guess the mean of a normal distribution
      p AnnealingState{asBestCandidate=AssessedCandidate{acFitness, acCandidate}} =
          show acCandidate <> " (" <> show acFitness <> ")"

  let acc AnnealingState{asBestCandidate=AssessedCandidate{acFitness}} = acFitness < 0.05
  S.head_ (S.dropWhile (not . acc) stream) >>= print
