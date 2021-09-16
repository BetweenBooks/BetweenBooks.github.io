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
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
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
      tags <- buildTagsWith getTags "updates/**" (fromCapture "tags/*.html")

      let ctx =  constField "commit"  commitDetails
              <> bookContext

      compile $ getResourceBody
                  >>= renderPandoc
                  >>= loadAndApplyTemplate "templates/review.html"  ctx
                  >>= loadAndApplyTemplate "templates/default.html" ctx
                  >>= applyAsTemplate ctx
                  >>= lqipImages imageMetaData
                  >>= relativizeUrls

      tagsRules tags $ \tag pattern -> do
        route idRoute
        compile $ do
          books    <- recentFirst =<< loadAll pattern
          tagCloud <- renderTagCloudWith makeLink (intercalate " ") 90 180 tags

          let tagCtx =  constField "tag"      tag
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


makeLink :: Double
         -> Double
         -> String
         -> String
         -> Int
         -> Int
         -> Int
         -> String
makeLink minSize maxSize tag url count min' max' =
    let diff     = 1 + fromIntegral max' - fromIntegral min'
        relative = (fromIntegral count - fromIntegral min') / diff
        size     = floor $ minSize + relative * (maxSize - minSize) :: Int
    in renderHtml $
        H.a ! A.style (toValue $ "font-size: " ++ show size ++ "%")
            ! A.href (toValue url)
            ! A.class_ (toValue $ tag ++ " tag")
            $ toHtml tag


bookContext :: Context String
bookContext 
  =  asList "authors"
  <> asList "tags"
  <> bbContext
    where
      -- HACK: Build a list from a list.
      asList fieldName
        = listFieldWith fieldName bbContext (\i -> do 
            let identifier = itemIdentifier i 
            metadata <- getMetadata identifier
            let metas = maybe [] id $ lookupStringList fieldName metadata
            return $ map (\x -> Item (fromFilePath x) x) (sort metas)
          )


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

