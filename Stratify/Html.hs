{-# LANGUAGE OverloadedStrings #-}
module Stratify.Html (page) where

import Text.Blaze
import Text.Blaze.Html5 (ToHtml, Html, docTypeHtml, link, (!), body, ul, li, toHtml)
import Text.Blaze.Html5.Attributes (rel, href, type_, class_)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Control.Monad (forM_)
import qualified Data.List as L
import Data.Ord

import Stratify.Types

page :: ToHtml a => [[Metrics (Cycle a)]] -> Html
page layers = docTypeHtml $ do
    H.head $ do
        H.title "Stratification"
        link ! rel "stylesheet" ! href "static/css/stratify.css" ! type_ "text/css"
    body $ do
        div ! class_ "colors-inlink"
        ul $ forM_ (reverse layers) ((li ! class_ "layer") . layer . sortLayer)
    where
        sortLayer = L.sortBy (flip $ comparing (datum . outlinks))

layer :: ToHtml a => [Metrics (Cycle a)] -> Html
layer deps = forM_ deps group

groups :: Double
groups = 20
groupNumber = floor . (* groups)

inlinkGroup m = "inlink-" ++ (show $ groupNumber $ fractionOfMax $ logInlinks m)
outlinkGroup m = "outlink-" ++  (show $ groupNumber $ percentile $ outlinks m)

group :: ToHtml a => Metrics (Cycle a) -> Html
group objects = H.span ! class_ (toValue $ L.intercalate " " ["group", inlinkGroup objects, outlinkGroup objects]) $ do
    H.span ! class_ "container" $ do
        forM_ (value objects) object
        H.div $ toHtml $ "Dependees: " ++ (show $ floor $ datum $ inlinks objects)
        H.div $ toHtml $ "Dependencies: " ++ (show $ floor $ datum $ outlinks objects)


object :: ToHtml a => a -> Html
object contents = H.div ! class_ "item" $ toHtml contents

