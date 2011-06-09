{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
module Stratify where

import Stratify.Internal
import Stratify.Html
import Stratify.Json
import Text.Blaze.Renderer.String
import Data.Aeson
import System.Console.CmdLib
import System.IO
import System.FilePath.Posix
import System.Directory
import System.Directory.Tree

translateJson :: String -> Either String String
translateJson input = case parseJsonFromString input of
    Left msg -> Left $ "Unable to parse json: "++msg
    Right value -> case parseDependenciesFromJson value of
        Left msg -> Left $ "Unable to create dependency tree from json: "++msg
        Right deps -> Right $ renderHtml $ page $ stratify deps

data Main = Main { output :: String, args :: [String], input :: String, template :: String }
     deriving (Typeable, Data, Eq)

instance Attributes Main where
    attributes _ = group "Options:" [
        output   %> [ Help "Directory to put the output. Will be created if it doesn't exist", ArgHelp "PATH", Default "output" ],
        template %> [ Help "Template directory containing static files", ArgHelp "PATH", Default "static" ],
        args     %> [ Extra True ],
        input    %> [ Positional 0, Default "-"]]

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

main = getArgs >>= executeR Main {} >>= \opts -> do
    inputData <- if input opts == "-"
        then getContents
        else readFile $ input opts
    page <- case translateJson inputData of
        Left msg -> error msg
        Right page -> return page

    let outputPage = joinPath [output opts, "index.html"]

    createDirectoryIfMissing True $ output opts
    writeFile outputPage page
    copyStaticContent (template opts) (joinPath [output opts, "static"])
