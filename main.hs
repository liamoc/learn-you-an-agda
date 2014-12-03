#!/usr/bin/runhaskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow((>>>))
import Hakyll
import Text.Pandoc.Options
import           Control.Applicative ((<$>))
import           Data.Monoid         ((<>))
import           Hakyll.Core.Compiler
import           Hakyll.Core.Configuration
import           Hakyll.Core.File
import           Hakyll.Core.Identifier
import           Hakyll.Core.Identifier.Pattern
import           Hakyll.Core.Item
import           Hakyll.Core.Metadata
import           Hakyll.Core.Routes
import           Hakyll.Core.Rules
import           Hakyll.Core.UnixFilter
import           Hakyll.Core.Writable
import           Hakyll.Main
import           Hakyll.Web.CompressCss
import           Hakyll.Web.Feed
import           Hakyll.Web.Html
import           Hakyll.Web.Html.RelativizeUrls
import           Hakyll.Web.Paginate
import           Hakyll.Web.Pandoc
import           Hakyll.Web.Pandoc.FileType
import           Hakyll.Web.Tags
import           Hakyll.Web.Template
import           Hakyll.Web.Template.Context
import           Hakyll.Web.Template.List
import Text.HTML.TagSoup
import System.Process
import System.IO.Temp
import qualified Data.ByteString.Base64 as B64
import System.IO
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BC
import Text.Pandoc.Options
import Text.Pandoc
import Debug.Trace
import Data.IORef
import System.IO.Unsafe
import Control.Monad
import System.Directory
import Control.Exception as E
import qualified Data.Set as S
import System.Exit
import System.FilePath
import Agda
import           Agda.Interaction.Options (CommandLineOptions(..), defaultOptions)


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
      compile $ let rs = (def { readerSmart = True, readerOldDashes = True, readerExtensions = S.insert Ext_pipe_tables pandocExtensions  }) 
                    ws = (def { writerHTMLMathMethod = GladTeX, writerHighlight = True }) 
                 in pandocAgdaCompilerWith (return . fmap (readMarkdown rs)) (writePandocWith ws) agdaOpts
             >>= loadAndApplyTemplate "templates/default.html" defaultContext
             >>= texCompiler
             >>= relativizeUrls
    match "toc.md" $ do
      route (setExtension "html")
      compile $ compiler >>=
                loadAndApplyTemplate "templates/toc.html" defaultContext

    match "index.md" $ do
      route (setExtension "html")

      compile $ compiler >>=
                loadAndApplyTemplate "templates/cover.html" defaultContext

defaultPreamble = ""
texCompiler :: Item String -> Compiler (Item String)
texCompiler it = do md <- getMetadata (itemIdentifier it)
                    let preamble = maybe defaultPreamble id (M.lookup "preamble" md)
                    withItemBody  (unsafeCompiler . go preamble) it

go :: String -> String -> IO String
go preamble = fmap (renderTagsOptions renderOpts) . sequence . convert preamble . parseTagsOptions parseOpts

convert :: String -> [Tag String] -> [IO (Tag String)]
convert preamble (TagOpen "EQ" [("ENV",env)] : TagText str : TagClose "EQ" : rest)
    = let mkAttribs = convertEquation str env preamble
          wrapper   = case env of "math" -> "span"; "displaymath" -> "div"
       in   return (TagOpen wrapper [("class",env)]) 
          : fmap (TagOpen "img") mkAttribs 
          : return (TagClose "img") 
          : return (TagClose wrapper)
          : convert preamble rest
convert preamble (x:xs) = return x : convert preamble xs
convert _ []     = []

renderOpts :: RenderOptions String
renderOpts = renderOptions 
  {optMinimize = (`elem` ["img","br","hr","meta","link","base","embed","param","area","col","input"]),
   optRawTag   = const True }

parseOpts :: ParseOptions String
parseOpts = parseOptionsEntities (const Nothing)

agdaOpts :: CommandLineOptions
agdaOpts = defaultOptions {optIncludeDirs = Left [".", "/Users/liamoc/.agda/lib/src/"]}

convertEquation :: String -> String -> String -> IO [Attribute String]
convertEquation eqn env preamble = do
  m <- (M.lookup (eqn, env) <$> readIORef cache)
  putStr $ "  image for equation \"" ++ take 10 eqn ++ "\"..."
  (img,w,h,style) <- case m of 
    Just it -> putStrLn " cached." >> return it
    Nothing -> do
      putStrLn " generating."
      (Just hin, Just hout, Just herr, _) <-
           createProcess (proc "eqn2img" ("-e":env:"-p":preamble:args)){ std_out = CreatePipe, std_err = CreatePipe, std_in = CreatePipe }
      hPutStr hin eqn
      hClose hin
      img <- B64.encode <$> B.hGetContents hout
      foo <- words <$> hGetContents herr      
      (w:h:style:rest) <- return foo
      if (null rest && read' w /= 0 && read' h /= 0 && read' style /= 10000) then
        modifyIORef cache (M.insert (eqn, env) (img,w,h,style)) 
       >>  return (img, w, h, style)
      else error $ unwords (w:h:style:rest)  
  return [("src", "data:image/png;base64," ++ BC.unpack img),("class",env ++ "_img"),("alt",eqn),("title",eqn),("width", show $ 1 + (read' w `div` 2)),("height", show $ read' h `div` 2),("style","margin:0;vertical-align:-" ++ show (read' style `div` 2) ++ "px;")]

args = ["-r","200", "-s","8", "-t", "-b","FFFFFF"]

cache :: IORef (M.Map (String, String) (B.ByteString, String, String, String))
{-# NOINLINE cache #-}
cache = unsafePerformIO (newIORef M.empty)

read' :: (Read a) => String -> a
read' x= read x

interruptible :: IO a -> IO a
interruptible m
  = E.handleJust f (const $ saveCache >> exitSuccess) m
  where
    f UserInterrupt = Just ()
    f _             = Nothing

saveCache = do
  x <- show <$> readIORef cache
  writeFile "cache.txt" x
loadCache = do
  v <- doesFileExist "cache.txt"
  when v $ (readFile "cache.txt" >>= \x -> return (read' x) ) >>= writeIORef cache
