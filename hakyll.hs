#!/usr/bin/runhaskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow((>>>))
import Hakyll   

main = hakyll $ do
    match "css/*" $ do
      route idRoute
      compile compressCssCompiler

    match "static/*" $ do
      route idRoute
      compile copyFileCompiler

    match "templates/*" $ do
      compile templateCompiler

    match "pages/*" $ do
      route (setExtension "html")
      compile $ pageCompiler >>> applyTemplateCompiler "templates/default.html" >>> relativizeUrlsCompiler

    match "toc.md" $ do
      route (setExtension "html")
      compile $ pageCompiler >>> applyTemplateCompiler "templates/toc.html" >>> relativizeUrlsCompiler

    match "index.md" $ do
      route (setExtension "html")
      compile $ pageCompiler >>> applyTemplateCompiler "templates/cover.html" >>> relativizeUrlsCompiler

