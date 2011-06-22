module Stratify.Types (
    NormalizedDependencies
  , Dependencies
  , normalize
  , Metrics (..)
  , Cycle
  , asDeps
  , revDeps
  , MetricsMap
  , Metric (..)
  ) where

import Data.List as L
import Data.Graph
import Data.Map as M
import Control.Exception
import Debug.Trace

data NormalizedDependencies a = Normalized {
    asDeps :: Dependencies (Cycle a),
    revDeps :: Dependencies (Cycle a)
    }
    deriving (Show)

type Dependencies a = Map a [a]
type Cycle a = [a]
type MetricsMap a = Map a (Metrics a)

normalize :: (Ord a, Show a) => Dependencies a -> NormalizedDependencies a
normalize d = traceShow ((keys deps) L.\\ (keys revDeps)) $ Normalized deps revDeps
    where
        deps = expandEmptyDeps $ collapseCycles d
        revDeps = insertEmpty (keys deps) $ reverseDeps deps

prependAt :: Ord k => v -> Map k [v] -> k -> Map k [v]
prependAt value = flip.alter $ maybe (Just [value]) (Just.(value:))

insertRevDep :: Ord a => a -> [a] -> Dependencies a -> Dependencies a
insertRevDep dep deps revDeps = foldl' (prependAt dep) revDeps deps

reverseDeps :: Ord a => Dependencies a -> Dependencies a
reverseDeps = foldrWithKey insertRevDep empty

insertIfMissing :: Ord k => k -> a -> Map k a -> Map k a
insertIfMissing = insertWith (flip const)

insertEmpty :: Ord a => [a] -> Dependencies a -> Dependencies a
insertEmpty keys deps = foldl' addMissing deps keys
    where
        addMissing deps key = insertIfMissing key [] deps

expandEmptyDeps :: Ord a => Dependencies a -> Dependencies a
expandEmptyDeps deps = insertEmpty (concat $ elems deps) deps

collapseCycles :: Ord a => Dependencies a -> Dependencies (Cycle a)
collapseCycles deps = fromList $ L.map groupAsDep $ groups
    where
        groupAsDep ds = (ds, minimalContainingComponents $ groupDependencies ds)
        groupDependencies = concatMap ((flip.findWithDefault) [] deps)
        minimalContainingComponents = nub . L.map ((flip.findWithDefault) [] containingComponent)
        containingComponent = fromList [(x, c) | c <- groups, x <- c]
        groups = L.map flattenSCC $ stronglyConnComp $ L.map nodeDep $ toList deps
        nodeDep (dep, deps) = (dep, dep, deps)

data Metric a = Metric {
      datum :: a
    , percentile :: Double
    , fractionOfMax :: Double
    }
    deriving (Show)

data Metrics a = Metrics {
      value :: a
    , inlinks :: Metric Double
    , outlinks :: Metric Double
    , logInlinks :: Metric Double
    , logOutlinks :: Metric Double
    }
    deriving (Show)
