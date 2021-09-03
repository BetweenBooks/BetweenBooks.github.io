{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DeriveGeneric     #-} 

import           Data.Aeson
import           Data.List
import           Data.String.Utils (strip)
import           Data.Maybe (fromJust, isJust, fromMaybe)
import           Data.String.Conv (toS)
import           GHC.Generics
import           Hakyll
import           Hakyll.Core.UnixFilter (unixFilter)
import           System.Environment (lookupEnv)
import           System.FilePath
import           Text.HTML.TagSoup (Tag (..))
import qualified Data.Map as M


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
  showDrafts    <- maybe False read <$> lookupEnv "SHOW_DRAFTS"

  hakyllWith config $ do 
    match "favicon.ico" $ do
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

    match "css/*.css" $ do
      route idRoute
      compile copyFileCompiler

    match "css/*.hs" $ do
      -- See: https://jaspervdj.be/hakyll/tutorials/using-clay-with-hakyll.html
      route   $ setExtension "css"

      let cssStr = getResourceString >>= withItemBody (unixFilter "stack" ["runghc"])
      compile $ fmap compressCss <$>  cssStr


    let draftCheck = if showDrafts then 
                        const True
                     else
                        \m -> lookupString "draft" m /= Just "draft"


    matchMetadata "updates/**.md" draftCheck $ do
      route $ setExtension "html"

      -- TODO: I don't quite know why this doesn't work.
      -- let getNonDraftTags :: MonadMetadata m => Identifier -> m [String]
      --     getNonDraftTags identifier = do
      --       metadata <- getMetadata identifier
      --       let isDraft = draftCheck metadata
      --       let tags    = fromMaybe [] $
      --                         (lookupStringList "tags" metadata) `mplus`
      --                         (map trim . splitAll "," <$> lookupString "tags" metadata)
      --       return $ if isDraft then [] else tags

      tags <- buildTagsWith getTags "updates/**" (fromCapture "tags/*.html")

      let ctx =  constField "commit" commitDetails
              <> tagsField  "tags"   tags
              <> bookContext

      compile $ getResourceBody
                  >>= renderPandoc
                  >>= loadAndApplyTemplate "templates/review.html"  ctx
                  >>= loadAndApplyTemplate "templates/default.html" ctx
                  >>= applyAsTemplate ctx
                  >>= lqipImages imageMetaData
                  >>= relativizeUrls

      tagsRules tags $ \tag pattern -> do
        let title = "Books tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
          books    <- recentFirst =<< loadAll pattern
          tagCloud <- renderTagCloud 90 180 tags

          let tagCtx =  constField "title"    title
                     <> listField  "books"    bookContext (return books)
                     <> constField "tagCloud" tagCloud
                     <> constField "commit"   commitDetails
                     <> bookContext

          makeItem ""
            >>= loadAndApplyTemplate "templates/tag.html"     tagCtx
            >>= loadAndApplyTemplate "templates/default.html" tagCtx
            >>= lqipImages imageMetaData
            >>= relativizeUrls

    match (fromList 
            [ "about.md"
            ]) $ do
      route $ setExtension "html"
      compile $ do
        let ctx =  constField "commit" commitDetails
                <> bbContext
      
        getResourceBody
          >>= renderPandoc
          >>= applyAsTemplate ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= lqipImages imageMetaData
          >>= relativizeUrls
  

    match "index.md" $ do
      route $ setExtension "html"
      compile $ do
        updates <- fmap (take 200) . recentFirst =<< loadAll "updates/**"

        let ctx =  constField "commit" commitDetails
                <> listField "books"   bookContext (return updates)
                <> bbContext
      
        getResourceBody
          >>= applyAsTemplate ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= lqipImages imageMetaData
          >>= relativizeUrls
  

bookContext :: Context String
bookContext =
  listFieldWith "authors" bbContext (\i -> do 
    let identifier = itemIdentifier i 
    metadata <- getMetadata identifier
    let metas = maybe [] id $ lookupStringList "authors" metadata
    return $ map (\x -> Item (fromFilePath x) x) metas
  )
  <> bbContext


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
    -- classes     = splitOn " " (fromMaybe "" $ M.lookup "class" attrDict)
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

