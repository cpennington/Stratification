import System.Directory.Tree
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import Distribution.PackageDescription
import Distribution.Package
import Stratify.Types
import Data.Aeson
import System
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M
import Stratify.Internal
import qualified Data.List as L
import Data.Ord

readHackageTree :: FilePath -> IO (DirTree GenericPackageDescription)
readHackageTree dir = do
    tree <- readDirectoryWithL (readPackageDescription silent) dir
    return $ free tree

isFile (File _ _) = True
isFile _ = False

isDir (Dir _ _) = True
isDir _ = False

instance Ord GenericPackageDescription where
    compare = comparing (package.packageDescription)

currentVersions :: DirTree GenericPackageDescription -> [GenericPackageDescription]
currentVersions (Dir root packages) = map file $ filter isFile currentVersionDirs
    where
        currentVersionDirs = concat $ map (contents . last . L.sort . contents) packageDirs
        packageDirs = filter isDir packages
currentVersions _ = []

constraintsFromComponent :: (Condition v, CondTree v c a, Maybe (CondTree v c a)) -> [c]
constraintsFromComponent (_, t, mt) = constraintsFromCondTree t

constraintsFromCondTree :: CondTree v c a -> [c]
constraintsFromCondTree tree = condTreeConstraints tree : (concat $ map constraintsFromComponent $ condTreeComponents tree)

allDependencies :: GenericPackageDescription -> [PackageName]
allDependencies pkg = map depName $ concat [fromBuild $ packageDescription pkg, fromLib $ condLibrary pkg, fromExecs $ condExecutables pkg]
    where
        fromBuild = buildDepends
        fromLib Nothing = []
        fromLib (Just tree) = concat $ constraintsFromCondTree tree
        fromExec (_, tree) = concat $ constraintsFromCondTree tree
        fromExecs = concat.(map fromExec)
        depName (Dependency pkgName _) = pkgName

dependencies :: [GenericPackageDescription] -> Dependencies PackageName
dependencies pkgs = M.fromList $ map getDeps pkgs
    where
        getDeps pkg = (packageName pkg, allDependencies pkg)
        packageName = pkgName . package . packageDescription

asStrings :: Dependencies PackageName -> Dependencies String
asStrings = (M.mapKeys asString).(M.map (map asString))
    where
        asString (PackageName name) = name

main :: IO ()
main = do
    args <- getArgs
    pkgs <- readHackageTree $ head args
    BS.putStr $ encode $ toJSON $ asStrings $ dependencies $ currentVersions pkgs
