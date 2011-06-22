module Stratify.Internal where

import Data.Map as M
import Data.List as L
import Stratify.Types
import Data.Ratio
import Debug.Trace

findShow :: (Ord k, Show k, Show a) => Map k a -> k -> a
findShow m k = findWithDefault (error $ "Unable to find " ++ show k ++ " in map " ++ show m) k m

satisfy :: Ord a => Dependencies a -> a -> Dependencies a -> (Dependencies a, [a])
satisfy revDep dep fwdDep = (M.delete dep remaining, nextLayer)
    where
        dependees = findWithDefault [] dep revDep
        nextLayer = [d | d <- dependees, findWithDefault [] d fwdDep == [dep]]
        remaining = foldl' (flip . adjust $ L.delete dep) fwdDep dependees

satisfyLayer :: Ord a => Dependencies a -> [a] -> Dependencies a -> (Dependencies a, [a])
satisfyLayer revDep layer deps = foldl' satisfy' (deps, []) layer
    where
        satisfy' (deps, nextLayer) dep = (deps', new ++ nextLayer)
            where (deps', new) = satisfy revDep dep deps


percentileMap xs = fromList $ zip (L.map head gs) percentiles
    where
        gs = group $ sort xs
        cumSums = scanl (+) 0 $ L.map genericLength gs
        percentiles = L.map (/ (genericLength xs)) cumSums

--computeMetricMap :: (Ord v, Show v) => Map k v -> Map k (Metric v)
computeMetricMap m = M.map metric m
    where
        max = maximum $ elems m
        percentiles = percentileMap $ elems m
        metric v = Metric {
              datum = v
            , percentile = findShow percentiles v
            , fractionOfMax = v / max
            }


computeMetrics :: (Ord a, Show a) => NormalizedDependencies a -> MetricsMap (Cycle a)
computeMetrics deps = M.mapWithKey metrics deps'
    where
        deps' = asDeps deps
        rDeps = revDeps deps
        inlinkMap = computeMetricMap $ M.map (genericLength) rDeps
        outlinkMap = computeMetricMap $ M.map (genericLength) deps'
        logInlinkMap = computeMetricMap $ M.map ((max 0) . log . genericLength) rDeps
        logOutlinkMap = computeMetricMap $ M.map ((max 0) . log . genericLength) deps'
        metrics dep _ = Metrics {
              value = dep
            , inlinks = findShow inlinkMap dep
            , outlinks = findShow outlinkMap dep
            , logInlinks = findShow logInlinkMap dep
            , logOutlinks = findShow logOutlinkMap dep
            }

stratify :: Ord a => NormalizedDependencies a -> [[Cycle a]]
stratify ds = unfoldr satisfyLayer' (ds', bottomLayer)
    where
        ds' = asDeps ds
        rds = revDeps ds
        bottomLayer = L.map fst $ L.filter (L.null . snd) $ toList ds'
        satisfyLayer' (deps, []) = if M.null deps
            then Nothing
            else Just (keys deps, (empty, []))
        satisfyLayer' (deps, layer) = Just (layer, (deps', layer'))
            where (deps', layer') = satisfyLayer rds layer deps
