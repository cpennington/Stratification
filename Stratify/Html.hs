{-# LANGUAGE OverloadedStrings #-}
module Stratify.Html (page) where

import Text.Blaze.Html5 (ToHtml, Html, docTypeHtml, link, (!), body, ul, li, toHtml)
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Control.Monad (forM_)

import Stratify.Types

page :: ToHtml a => [[Cycle a]] -> Html
page layers = docTypeHtml $ do
    H.head $ do
        H.title "Stratification"
        link ! rel "stylesheet" ! href "static/css/stratify.css" ! type_ "text/css"
    body $ do
        ul $ forM_ (reverse layers) ((li ! class_ "layer") . layer)

layer :: ToHtml a => [Cycle a] -> Html
layer deps = forM_ deps group

group :: ToHtml a => Cycle a -> Html
group objects = H.span ! class_ "group" $ do
    H.span ! class_ "container" $ do
        forM_ objects object

object :: ToHtml a => a -> Html
object contents = H.span ! class_ "item" $ toHtml contents

