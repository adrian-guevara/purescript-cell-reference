module Spec where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.List (List(..))
import Data.List as List
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)
import Test.QuickCheck (Seed, unSeed, ResultSummary, checkResults, quickCheckPure', class Testable, randomSeed)
import Test.Spec.Assertions (fail)
import Optional (class Optional, optional, default)

type ConfigFields =
  ( initialSeed :: Seed
  , testCases :: Int
  , formatOnFail :: ResultSummary -> String
  )

type Config = Record ConfigFields

type OptionalConfigFields =
  ( initialSeed :: Maybe Seed
  , testCases :: Maybe Int
  , formatOnFail :: Maybe (ResultSummary -> String)
  )

type OptionalConfig = Maybe (Record OptionalConfigFields)

defaultConfig :: Seed -> Config
defaultConfig initialSeed =
  { initialSeed
  , testCases: 100
  , formatOnFail: formatShort
  }

resolveConfig
  :: forall m cfg
   . MonadEffect m
  => Optional Config cfg OptionalConfig
  => cfg
  -> m Config
resolveConfig cfg = do
  let
    initialSeedOption =
      case optional @Config cfg of
        Nothing -> Nothing
        Just options -> options.initialSeed
  initialSeed <- resolveSeed initialSeedOption
  pure $ default (defaultConfig initialSeed) cfg

resolveSeed :: forall m. MonadEffect m => Maybe Seed -> m Seed
resolveSeed (Just initialSeed) = pure initialSeed
resolveSeed Nothing = liftEffect randomSeed

formatShort :: ResultSummary -> String
formatShort { successes, total } =
  show successes
    <> "/"
    <> show total
    <> " test"
    <>
      if successes == 1 then ""
      else "s"
        <> " passed"

formatFull :: ResultSummary -> String
formatFull summary@{ failures } =
  formatShort summary
    <> foldMap formatFailure (List.reverse failures)
  where
  formatFailure { index, seed, message } =
    "\ntest "
      <> show (index + 1)
      <> " (seed "
      <> show (unSeed seed)
      <> "): "
      <> message

reportResults
  :: forall m
   . MonadThrow Error m
  => (ResultSummary -> String)
  -> ResultSummary
  -> m Unit
reportResults formatOnFail summary =
  case summary.failures of
    Nil ->
      pure unit
    _ ->
      fail $ formatOnFail summary

check
  :: forall m prop
   . MonadThrow Error m
  => MonadEffect m
  => Testable prop
  => prop
  -> m Unit
check = checkWithConfig {}

checkWithConfig
  :: forall m prop cfg
   . MonadThrow Error m
  => MonadEffect m
  => Testable prop
  => Optional Config cfg OptionalConfig
  => cfg
  -> prop
  -> m Unit
checkWithConfig config prop = do
  { initialSeed, testCases, formatOnFail } <- resolveConfig config
  let results = quickCheckPure' initialSeed testCases prop
  let summary = checkResults results
  reportResults formatOnFail summary