{-# LANGUAGE OverloadedStrings, LambdaCase #-}

import Data.Maybe

import Image.LaTeX.Render
import Image.LaTeX.Render.Pandoc

import Network.URI

import Text.Pandoc

import Hakyll
import Hakyll.Contrib.Agda
import Hakyll.Contrib.LaTeX

import qualified Data.Map as M
import qualified Data.Set as S

agdaOpts :: CommandLineOptions
agdaOpts = defaultOptions

defaultPreamble :: String
defaultPreamble = ""

formulaSettings :: String -> PandocFormulaOptions
formulaSettings pre
  = defaultPandocFormulaOptions
      { formulaOptions = \case DisplayMath -> displaymath { preamble = pre }
                               _           -> math        { preamble = pre }
      }

readerSettings :: ReaderOptions
readerSettings = def { readerSmart      = True
                     , readerOldDashes  = True
                     , readerExtensions = S.insert Ext_pipe_tables pandocExtensions
                     }

writerSettings :: WriterOptions
writerSettings = def { writerHighlight = True }

agdaLibURI :: URI
Just agdaLibURI = parseURIReference "/agda-lib/"

main :: IO ()
main = do
  renderEquations <- initFormulaCompilerDataURI 1000 defaultEnv
  hakyll $ do
    match "css/*" $ do
      route idRoute
      compile compressCssCompiler

    match "static/*" $ do
      route idRoute
      compile copyFileCompiler

    match "templates/*" $
      compile templateCompiler

    match ("pages/*.md" .||. "pages/*.lagda") $ do
      route (setExtension "html")
      compile $ do
        pr <- fmap (fromMaybe defaultPreamble . M.lookup "preamble") (getMetadata =<< getUnderlying)
        literateAgdaCompilerWithTransformM agdaOpts Markdown agdaLibURI readerSettings writerSettings
                 (traverse $ renderEquations $ formulaSettings pr)
             >>= loadAndApplyTemplate "templates/default.html" defaultContext
             >>= relativizeUrls

    match "toc.md" $ do
      route (setExtension "html")
      compile $ pandocCompiler >>=
                loadAndApplyTemplate "templates/toc.html" defaultContext

    match "index.md" $ do
      route (setExtension "html")
      compile $ pandocCompiler >>=
                loadAndApplyTemplate "templates/cover.html" defaultContext

