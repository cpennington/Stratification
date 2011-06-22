{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
module Main where

import Stratify.Internal
import Stratify.Html
import Stratify.Json
import Stratify.Types
import Text.Blaze.Renderer.String
import Data.Aeson
import System.Console.CmdLib
import System.IO
import System.FilePath.Posix
import System.Directory
import System.Directory.Tree
import System.Posix.Files
import Data.Map ((!))
import Debug.Trace

renderJsonAsHtml :: String -> Either String String
renderJsonAsHtml input = case parseJsonFromString input of
    Left msg -> Left $ "Unable to parse json: "++msg
    Right value -> case parseDependenciesFromJson value of
        Left msg -> Left $ "Unable to create dependency tree from json: "++msg
        Right deps -> let
            ndeps = normalize deps
            stratified = stratify ndeps
            metrics = computeMetrics ndeps
            in Right $ renderHtml $ page ((map . map) (findShow metrics) stratified)

data Main = Main {
    output :: String,
    args :: [String],
    input :: String,
    template :: String,
    develop :: Bool
    }
    deriving (Typeable, Data, Eq)

instance Attributes Main where
    attributes _ = group "Options:" [
        output   %> [ Help "Directory to put the output. Will be created if it doesn't exist", ArgHelp "PATH", Default "output" ],
        template %> [ Help "Template directory containing static files", ArgHelp "PATH", Default "static" ],
        args     %> [ Extra True ],
        input    %> [ Positional 0, Default "-" ],
        develop  %> [ Help "Link static directory, rather than copying" ]]

instance RecordCommand Main where
     mode_summary _ = "Generate a webpage of stratified dependencies from an input file"

copyStaticDir :: FilePath -> FilePath -> FilePath -> IO ()
copyStaticDir templateDir outputDir name = do
    sourceDir <- readDirectoryWithL readFile (joinPath [templateDir, name])
    failedFiles <- writeDirectory $ outputDir :/ free sourceDir
    return ()

copyStaticContent :: FilePath -> FilePath -> IO ()
copyStaticContent template output = do
    createDirectoryIfMissing True output
    copyStaticDir template output "js"
    copyStaticDir template output "css"

linkStaticContent :: FilePath -> FilePath -> IO ()
linkStaticContent template output = do
    template' <- canonicalizePath template
    createSymbolicLink template' output

main = getArgs >>= executeR Main {} >>= \opts -> do
    inputData <- if input opts == "-"
        then getContents
        else readFile $ input opts
    page <- case renderJsonAsHtml inputData of
        Left msg -> error msg
        Right page -> return page

    let outputPage = joinPath [output opts, "index.html"]

    createDirectoryIfMissing True $ output opts
    writeFile outputPage page
    let distributeContent = if develop opts then linkStaticContent else copyStaticContent
    distributeContent (template opts) (joinPath [output opts, "static"])
