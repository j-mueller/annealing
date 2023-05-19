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

import           Control.DeepSeq               (NFData)
import           Control.Monad.Primitive       (PrimState)
import           Data.Foldable                 (traverse_)
import qualified Data.List                     as List
import           GHC.Generics                  (Generic)
import           Streaming
import qualified Streaming.Prelude             as S
import qualified System.Random.MWC.Probability as P
import           System.Random.MWC.Probability (Gen)

{-| A candidate solution with its fitness
-}
data AssessedCandidate s r =
  AssessedCandidate
    { acCandidate :: !s
    , acResult    :: !r
    , acFitness   :: !Double
    }
    deriving stock (Show, Generic)
    deriving anyclass (NFData)

{-| Annealing state
-}
data AnnealingState candidate =
  AnnealingState
    { asTime             :: !Int
    , asBestCandidate    :: !candidate
    , asCurrentCandidate :: !candidate
    , asLastProbability  :: !(Maybe Double)
    , asLastTemperature  :: !Double
    } deriving Show

initialState :: candidate -> AnnealingState candidate
initialState candidate = AnnealingState 1 candidate candidate Nothing 0

{-| The simulated annealing algorithm. It generates an infinite stream of
  'AnnealingState' values. Use @S.head_ $ S.dropWhile (not . accept)@ get
  the first solution that meets the acceptance criterion @accept@.
-}
annealing ::
  forall state result.
  Gen (PrimState IO)
  -> (Int -> Double) -- ^ Temperature
  -> (state -> IO result) -- ^ Evaluation of current state
  -> (result -> Double) -- ^ Fitness function. 0 = perfect
  -> AssessedCandidate state result -- ^ Initial state
  -> (state -> Stream (Of state) IO ()) -- ^ Neighbour selection
  -> Stream (Of (AnnealingState (AssessedCandidate state result))) IO () -- ^ Successive approximations
annealing gen temp eval fitness s neighbours = flip S.unfoldr (initialState s) $ \AnnealingState{asTime, asBestCandidate, asCurrentCandidate, asLastProbability} -> do
  let currentTemp = temp asTime
      currentFit = acFitness asCurrentCandidate
      assess c = do
        k <- eval c
        pure $ AssessedCandidate c k (fitness k)

  let bestNeighbour :: IO (AssessedCandidate state result)
      bestNeighbour = do
        list <- S.toList_ $ S.mapM assess $ S.take 8 $ neighbours (acCandidate asCurrentCandidate)
        return $ head $ List.sortOn acFitness list

      accept :: AssessedCandidate state result -> IO (Maybe (AssessedCandidate state result), Maybe Double)
      accept AssessedCandidate{acCandidate=y, acFitness=newFit, acResult=yr} = do
        if newFit <= currentFit
          then return $ (Just (AssessedCandidate y yr newFit), Nothing)
          else do
            let r    = negate ((newFit - currentFit) / currentTemp)
                rate = exp r
            shouldAccept <- P.sample (P.bernoulli rate) gen
            if shouldAccept
              then return (Just (AssessedCandidate y yr newFit), Just rate)
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
  let fitness s = abs (150 - s) -- fitness = deviation from actual mean
      stream =
        annealing
          gen
          (\i -> 1 + (1 / fromIntegral i))
          pure
          fitness
          (AssessedCandidate 0 0 (fitness 0))
          (\_ -> S.repeatM (P.sample (P.normal 150 15) gen)) -- try to guess the mean of a normal distribution
      p AnnealingState{asBestCandidate=AssessedCandidate{acFitness, acCandidate}} =
          show acCandidate <> " (" <> show acFitness <> ")"

  let accept AnnealingState{asBestCandidate=AssessedCandidate{acFitness}} = acFitness < 0.05
  S.head_ (S.dropWhile (not . accept) stream) >>= traverse_ (putStrLn . p)
