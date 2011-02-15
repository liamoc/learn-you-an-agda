#!/usr/bin/runhaskell
import Text.Hakyll   
import Text.Hakyll.Render
import Text.Hakyll.File
import Text.Hakyll.CreateContext
import Text.Pandoc.Shared

withHeader = (`combine` createPage "header.markdown")
withFooter = (`combine` createPage "footer.markdown")

main = hakyllWithConfiguration conf $ do
    directory static "static"
    directory css "css"
    directory render "pages"
    where render = renderChain ["templates/default.html"]
                 . withHeader
                 . withFooter
                 . createPage
          conf = (defaultHakyllConfiguration "http://learnyouanagda.com") { pandocWriterOptions = defaultWriterOptions { writerHTMLMathMethod = GladTeX } }
