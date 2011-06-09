{-# LANGUAGE OverloadedStrings #-}
module Stratify.Html where

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Control.Monad (forM_)

import Stratify.Types

page layers = docTypeHtml $ do
    H.head $ do
        H.title "Stratification"
    body $ do
        ul $ forM_ (reverse layers) (li . layer)

layer deps = forM_ deps (H.span . toHtml)


