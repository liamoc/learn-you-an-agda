#!/usr/bin/runhaskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow((>>>))
import Hakyll
import Text.Pandoc.Options
compiler = pandocCompilerWith r w >>= relativizeUrls
  where r = defaultHakyllReaderOptions
        w = defaultHakyllWriterOptions { writerHTMLMathMethod = MathJax "https://c328740.ssl.cf1.rackcdn.com/mathjax/latest/MathJax.js" }

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
      compile $ compiler >>=
                loadAndApplyTemplate "templates/default.html" defaultContext
    match "toc.md" $ do
      route (setExtension "html")
      compile $ compiler >>=
                loadAndApplyTemplate "templates/toc.html" defaultContext

    match "index.md" $ do
      route (setExtension "html")
      compile $ compiler >>=
                loadAndApplyTemplate "templates/cover.html" defaultContext

