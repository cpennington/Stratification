module Stratify.Types (
    NormalizedDependencies
  , Dependencies
  , normalize
  , Metrics (..)
  , Cycle
  , asDeps
  , MetricsMap
  ) where

import Data.List as L
import Data.Graph
import Data.Map as M

newtype NormalizedDependencies a = Normalized { asDeps :: Dependencies (Cycle a) }

type Dependencies a = Map a [a]
type Cycle a = [a]
type MetricsMap a = Map a (Metrics a)

normalize :: Ord a => Dependencies a -> NormalizedDependencies a
normalize = Normalized . collapseCycles . expandEmptyDeps

expandEmptyDeps :: Ord a => Dependencies a -> Dependencies a
expandEmptyDeps deps = foldl' addMissing deps $ concat $ elems deps
    where
        addMissing deps key = insertWith (flip const) key [] deps

collapseCycles :: Ord a => Dependencies a -> Dependencies (Cycle a)
collapseCycles deps = fromList $ L.map groupAsDep $ groups
    where
        groupAsDep ds = (ds, minimalContainingComponents $ groupDependencies ds)
        groupDependencies = concatMap ((flip.findWithDefault) [] deps)
        minimalContainingComponents = nub . L.map ((flip.findWithDefault) [] containingComponent)
        containingComponent = fromList [(x, c) | c <- groups, x <- c]
        groups = L.map flattenSCC $ stronglyConnComp $ L.map nodeDep $ toList deps
        nodeDep (dep, deps) = (dep, dep, deps)

data Metrics a = Metrics {
    value :: a,
    inlinks :: Int,
    outlinks :: Int,
    inlinksPercentile :: Rational,
    outlinksPercentile :: Rational
    }
