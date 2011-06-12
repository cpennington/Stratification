module Stratify.Internal where

import Data.Map as M
import Data.List as L
import Stratify.Types
import Data.Ratio

prependAt :: Ord k => v -> Map k [v] -> k -> Map k [v]
prependAt value = flip.alter $ maybe (Just [value]) (Just.(value:))

insertRevDep :: Ord a => a -> [a] -> Dependencies a -> Dependencies a
insertRevDep dep deps revDeps = foldl' (prependAt dep) revDeps deps

reverseDeps :: Ord a => Dependencies a -> Dependencies a
reverseDeps = foldrWithKey insertRevDep empty

satisfy :: Ord a => a -> Dependencies a -> (Dependencies a, [a])
satisfy dep fwdDep = (M.delete dep remaining, nextLayer)
    where
        revDep = reverseDeps fwdDep
        dependees = findWithDefault [] dep revDep
        nextLayer = [d | d <- dependees, findWithDefault [] d fwdDep == [dep]]
        remaining = foldl' (flip . adjust $ L.delete dep) fwdDep dependees

satisfyLayer :: Ord a => [a] -> Dependencies a -> (Dependencies a, [a])
satisfyLayer layer deps = foldl' satisfy' (deps, []) layer
    where
        satisfy' (deps, nextLayer) dep = (deps', new ++ nextLayer)
            where (deps', new) = satisfy dep deps


percentileMap xs = fromList $ zip (L.map head gs) percentiles
    where
        gs = group $ sort xs
        cumSums = scanl (+) 0 $ L.map length gs
        percentiles = L.map (% (length xs)) cumSums

computeMetrics :: Ord a => NormalizedDependencies a -> MetricsMap (Cycle a)
computeMetrics deps = M.mapWithKey metrics deps'
    where
        deps' = asDeps deps
        revDeps = reverseDeps deps'
        inlinkCounts = M.map length deps'
        outlinkCounts = M.map length revDeps
        inPercentiles = percentileMap $ elems inlinkCounts
        outPercentiles = percentileMap $ elems outlinkCounts
        metrics dep _ = let
                outlinks = outlinkCounts ! dep
                inlinks = inlinkCounts ! dep
            in Metrics {
              value = dep
            , inlinks = inlinks
            , outlinks = outlinks
            , inlinksPercentile = toRational $ inPercentiles ! inlinks
            , outlinksPercentile = toRational $ outPercentiles ! outlinks
            }

stratify :: Ord a => NormalizedDependencies a -> [[Cycle a]]
stratify ds = unfoldr satisfyLayer' (ds', bottomLayer)
    where
        ds' = asDeps ds
        bottomLayer = L.map fst $ L.filter (L.null . snd) $ toList ds'
        satisfyLayer' (deps, []) = if M.null deps
            then Nothing
            else Just (keys deps, (empty, []))
        satisfyLayer' (deps, layer) = Just (layer, (deps', layer'))
            where (deps', layer') = satisfyLayer layer deps
