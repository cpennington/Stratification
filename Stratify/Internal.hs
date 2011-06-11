module Stratify.Internal where

import Data.Map as M
import Data.List as L
import Stratify.Types
import Data.Graph

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

stratify :: Ord a => Dependencies a -> [[Cycle a]]
stratify ds = unfoldr satisfyLayer' (ds', bottomLayer)
    where
        ds' = collapseCycles $ expandEmptyDeps ds
        bottomLayer = L.map fst $ L.filter (L.null . snd) $ toList ds'
        satisfyLayer' (deps, []) = if M.null deps
            then Nothing
            else Just (keys deps, (empty, []))
        satisfyLayer' (deps, layer) = Just (layer, (deps', layer'))
            where (deps', layer') = satisfyLayer layer deps
