--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
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
import           BiblioWorkaround
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

args = ["-r","200", "-s","8", "-t", "-b","FFFFFF"]

cache :: IORef (M.Map (String, String) (B.ByteString, String, String, String))
{-# NOINLINE cache #-}
cache = unsafePerformIO (newIORef M.empty)

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

renderOpts :: RenderOptions String
renderOpts = renderOptions 
  {optMinimize = (`elem` ["img","br","hr","meta","link","base","embed","param","area","col","input"]),
   optRawTag   = const True }

parseOpts :: ParseOptions String
parseOpts = parseOptionsEntities (const Nothing)

go :: String -> String -> IO String
go preamble = fmap (renderTagsOptions renderOpts) . sequence . convert preamble . parseTagsOptions parseOpts

agdaOpts :: CommandLineOptions
agdaOpts = defaultOptions {optIncludeDirs = Left [".", "/Users/liamoc/.agda/lib/src/"]}

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

defaultPreamble = ""
texCompiler :: Item String -> Compiler (Item String)
texCompiler it = do md <- getMetadata (itemIdentifier it)
                    let preamble = maybe defaultPreamble id (M.lookup "preamble" md)
                    withItemBody  (unsafeCompiler . go preamble) it

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "liamoc.net"
    , feedDescription = "Musings of Liam O'Connor, lambda scientist"
    , feedAuthorName  = "Liam O'Connor"
    , feedAuthorEmail = "me@liamoc.net"
    , feedRoot        = "http://liamoc.net"
    }


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


    
read' :: (Read a) => String -> a
read' x= read x
--------------------------------------------------------------------------------
main :: IO ()
main = do 
  loadCache 
  interruptible $ hakyll $ do
    tags <- buildTags ("posts/*.markdown" .||. "posts/*.lagda" .||. "posts/*.org") (fromCapture "tags/*.html")

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "agda-lib/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/patterns/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "cites/*.bib" $ compile $ biblioCompiler
    match "association-for-computing-machinery.csl" $ compile $ cslCompiler

    match ("posts/*.markdown" .||. "posts/*.lagda" .||. "posts/*.org") $ do
        route $ setExtension "html"

        compile $ do
            fp <-  flip replaceExtension "bib" . flip replaceDirectory "cites/" <$> getResourceFilePath            
            pandocCompilerWith' <- if (unsafePerformIO $ doesFileExist fp)  then do 
              csl <- load "association-for-computing-machinery.csl"
              bib <- load (fromFilePath fp)
              return $ \rs ws -> pandocAgdaCompilerWith (readPandocBiblio (fmap $ readMarkdown rs) csl bib)  (writePandocWith ws)
             else return $ \rs ws -> pandocAgdaCompilerWith (return . fmap (readMarkdown rs)) (writePandocWith ws) 
            pandocCompilerWith' (def { readerSmart = True, readerOldDashes = True, readerExtensions = S.insert Ext_pipe_tables pandocExtensions  }) (def { writerHTMLMathMethod = GladTeX, writerHighlight = True }) agdaOpts
             >>= loadAndApplyTemplate "templates/post.html"    (tagsCtx tags)
             >>= texCompiler
             >>= saveSnapshot "content"
             >>= loadAndApplyTemplate "templates/disqus.html"  (tagsCtx tags)
             >>= loadAndApplyTemplate "templates/default.html" (tagsCtx tags)
             >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            let archiveCtx = field "posts" (\_ -> postList' recentFirst)
                           <> constField "title" "Archives"
                           <> defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag
        route idRoute
        compile $ do
            list <- postList tags pattern recentFirst
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html"
                        (constField "title" title <>
                            constField "posts" list <>
                            defaultContext)
                >>= loadAndApplyTemplate "templates/default.html"
                        (constField "title" title <>
                            defaultContext)
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            let indexCtx = field "posts" (const $ postList' (\x -> take 3 <$> recentFirst x))
                         <> field "tagcloud" (const $ renderTagCloud 75 150 tags)
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            posts <- take 10  <$> (recentFirst =<< loadAllSnapshots "posts/*" "content")
            renderAtom feedConfiguration feedCtx posts

    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            posts <- take 10 <$> (recentFirst =<< loadAllSnapshots "posts/*" "content")
            renderRss feedConfiguration feedCtx posts

    match "templates/*" $ compile templateCompiler




--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext

feedCtx :: Context String
feedCtx = postCtx <> bodyField "description"

--------------------------------------------------------------------------------
postList' :: ([Item String] -> Compiler [Item String]) -> Compiler String
postList' sortFilter = do
    posts   <- sortFilter =<< loadAll "posts/*"
    itemTpl <- loadBody "templates/post-item.html"
    list    <- applyTemplateList itemTpl postCtx posts
    return list

postList :: Tags -> Pattern -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList tags pattern preprocess' = do
    postItemTpl <- loadBody "templates/post-item.html"
    posts <- preprocess' =<< loadAll pattern
    applyTemplateList postItemTpl (tagsCtx tags) posts

tagsCtx :: Tags -> Context String
tagsCtx tags =
    tagsField "taglinks" tags <>
    postCtx
