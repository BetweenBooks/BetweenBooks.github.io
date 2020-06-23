{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DeriveGeneric     #-} 

import           Control.Applicative (empty)
import           Control.Monad (liftM, filterM, forM_, when)
import           Data.Aeson
import           Data.Functor
import           Data.List
import           Data.String.Utils (strip)
import           Data.List.Split (splitOn)
import           Data.Maybe (fromJust, isJust, fromMaybe)
import           Data.Monoid (mappend)
import           Data.String.Conv (toS)
import           Data.Traversable
import           GHC.Generics
import           Hakyll
import           Hakyll.Core.UnixFilter (unixFilter)
import           Hakyll.Web.Redirect
import           Hakyll.Web.Sass (sassCompiler)
import           System.Directory
import           System.Environment (lookupEnv)
import           System.FilePath
import           Text.HTML.TagSoup (Tag (..))
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S


markdownCompiler =
    pandocCompilerWith 
      defaultHakyllReaderOptions 
      defaultHakyllWriterOptions

sortByIds :: MonadMetadata m => [Item a] -> m [Item a]
sortByIds items = fmap last . sortBy f <$> indexedIdents
  where 
    f (a, b, c, _) (x, y, z, _) = compare (a, b, c) (x,y, z)
    ids = map itemIdentifier items
    indexedIdents = for (zip ids items) $ \(ident, item) -> do
      chIdx    <- getMetadataField' ident "index" <&> read @Int
      chSec    <- getMetadataField' ident "section" <&> read @Int
      chSubSec <- getMetadataField' ident "subsection" <&> read @Int
      pure (chIdx, chSec, chSubSec, item)
    last (_, _, _, z) = z


sortIds :: MonadMetadata m => [Identifier] -> m [Identifier]
sortIds ids = liftM (map itemIdentifier) $ sortByIds [Item i () | i <- ids]


ourPaginator :: MonadMetadata m => Pattern -> m Paginate
ourPaginator contentMatcher = do
  ms  <- getAllMetadata contentMatcher
  ids <- sortIds $ (\(ident, meta) -> ident) <$> ms
  let pageId n = ids !! (n - 1) -- start indices at 0
      grouper = pure . fmap (\p -> [p])
  buildPaginateWith grouper contentMatcher pageId


isRegularFileOrDirectory :: FilePath -> Bool
isRegularFileOrDirectory f = f /= "." && f /= ".."


config :: Configuration
config = defaultConfiguration
    { ignoreFile = ignoreFile'
    }
  where
    ignoreFile' path
        | "."     `isPrefixOf` fileName = True
        | "#"     `isPrefixOf` fileName = True
        | "~"     `isSuffixOf` fileName = True
        | ".swp"  `isSuffixOf` fileName = True
        --
        -- For git annoyances related to zsh.
        -- 
        | "/.git/" `isInfixOf` path     = True
        | otherwise                     = False
      where
        fileName = takeFileName path


main :: IO ()
main = do
  commitDetails <- strip <$> readFile "metadata/gitinfo"
  imageMetaData <- computeImageMetaData
  -- allFolders    <- filter isRegularFileOrDirectory <$> getDirectoryContents "content/"

  -- If we've been provided a specific folder, use that, otherwise
  -- run generation for all of them.
  -- folders   <- maybe allFolders (:[]) <$> lookupEnv "SPECIFIC_FOLDER"
  fastBuild <- maybe False read <$> lookupEnv "FAST_BUILD"

  hakyllWith config $ do 
    match "etc/*" $ do
      route idRoute
      compile copyFileCompiler

    match "templates/*" $ do
      route idRoute
      compile templateBodyCompiler

    match "images/**" $ do
      route idRoute
      compile copyFileCompiler

    match "js/**" $ do
      route idRoute
      compile copyFileCompiler

    match "css/*.hs" $ do
      -- See: https://jaspervdj.be/hakyll/tutorials/using-clay-with-hakyll.html
      route   $ setExtension "css"
      let cssStr = getResourceString >>= withItemBody (unixFilter "stack" ["runghc"])
      compile $ (fmap compressCss) <$>  cssStr


    match (fromList ["about.md", "index.md"]) $ do
      route $ setExtension "html"
      compile $ do
        let ctx = bbContext
                  <> constField "commit" commitDetails

        markdownCompiler
          >>= applyAsTemplate ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= lqipImages imageMetaData
  

    -- let pages = (\x -> "content/" ++ x ++ "/*") <$> folders

    -- Create all the sub-sites ...
    -- forM_ pages $ \pageGlob -> do
    --   let contentMatcher = fromGlob pageGlob

    --   paginator <- ourPaginator contentMatcher

    --   when (not fastBuild) $ do
    --     match contentMatcher $ version "precomp" $ do
    --       route $ setExtension "html"
    --       compile $ pandocCompiler

    --   paginateRules paginator $ \page _ -> do
    --       route $ setExtension "html"
    --       compile $ do
    --         pages <- sortByIds =<< loadAll (contentMatcher .&&. hasVersion "precomp")

    --         let ctx = ourContext
    --               <> listField "pages" ourContext (return pages)
    --               <> constField "commit" commitDetails
    --               <> paginateContext paginator page
                
    --         pandocCompiler
    --           >>= loadAndApplyTemplate "templates/content.html" ctx
    --           >>= loadAndApplyTemplate "templates/default.html" ctx
    --           >>= lqipImages imageMetaData
    --           >>= relativizeUrls
      --
      -- create [ fromFilePath $ init pageGlob ++ "index.html" ] $ do
      --   route idRoute
      --   compile $ makeItem $ Redirect "front.html"


bbContext :: Context String
bbContext = 
  constField "rootUrl" "https://betweenbooks.com.au"
  <> dateField "date" "%B %e, %Y"
  <> defaultContext


lqipImages :: ImageMetaDataMap -> Item String -> Compiler (Item String)
-- No LQIP
-- lqipImages imageMetaData = return . fmap id
-- Full LQIP
lqipImages imageMetaData = return . fmap (withTags . switchInLqipImages $ imageMetaData)


data ImageData = ImageData 
  { base64String :: String
  , width        :: Int
  , height       :: Int
  , name         :: String
  } deriving (Generic)


instance FromJSON ImageData

type ImageMetaDataMap = M.Map String ImageData


computeImageMetaData :: IO (ImageMetaDataMap)
computeImageMetaData = do
  items <- lines <$> readFile "./metadata/images.jsonl"

  let decoded' :: [Maybe ImageData]
      decoded' = map (decode' . toS) items
      decoded  = map fromJust (filter isJust decoded')
  
  return $ M.fromList (map (\i -> (name i, i)) decoded)


switchInLqipImages :: ImageMetaDataMap -> (Tag String -> Tag String)
switchInLqipImages imageMetaDataMap t@(TagOpen "img" attrs) = newTag
  where
    doLqip      = True -- Could be condition on some class.
    classes     = splitOn " " (fromMaybe "" $ M.lookup "class" attrDict)
    attrDict    = M.fromList attrs
    nonSrcAttrs = [ (k, v) | (k, v) <- attrs, v /= "src" ]
    --
    src       = fromMaybe (error $ "No source for tag: " ++ show t) (M.lookup "src" attrDict)
    imageData = M.lookup (drop 1 src) imageMetaDataMap
    script    = ("onload", "this.src = '" ++ src ++ "'; this.onload = null;")
    --
    newAttrs  = (\d -> script : ("src", "data:image/png;base64," ++ base64String d) : nonSrcAttrs) <$> imageData
    newTag    = case newAttrs of
                  Nothing -> t
                  Just nt -> if doLqip then (TagOpen "img" nt) else t
switchInLqipImages _ t = t

