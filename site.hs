{-# language DeriveGeneric     #-}
{-# language FlexibleContexts  #-}
{-# language OverloadedStrings #-}
{-# language TupleSections     #-}
{-# language TypeApplications  #-}

import           Control.Monad                   (liftM)
import           Data.Aeson
import           Data.List
import           Data.List.Split                 (splitOn)
import           Data.Maybe                      (fromJust, isJust, fromMaybe)
import           Data.Ord                        (comparing)
import           Data.String.Conv                (toS)
import           Data.String.Utils               (strip)
import           Data.Time.Clock                 (UTCTime (..))
import           Data.Time.Format                (parseTimeM)
import           Data.Time.Locale.Compat         (TimeLocale, defaultTimeLocale)
import           GHC.Generics
import           Hakyll
import           System.Environment              (lookupEnv)
import           System.FilePath
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.HTML.TagSoup (             Tag (..))
import qualified Data.Map                        as M
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A


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
      route $ setExtension "css"

      let cssStr = getResourceString >>= withItemBody (unixFilter "runghc" [])

      compile $ fmap compressCss <$> cssStr


    let draftCheck = if showDrafts then
                        const True
                     else
                        \m -> lookupString "draft" m /= Just "draft"

    -- ~ Shelf updates
    match "shelf/**.md" $ do
      route $ setExtension "html"

      let ctx =  constField "commit"  commitDetails
              <> bookContext

      compile $ getResourceBody
                  >>= renderPandoc
                  >>= loadAndApplyTemplate "templates/shelf.html"  ctx
                  >>= loadAndApplyTemplate "templates/default.html" ctx
                  >>= applyAsTemplate ctx
                  >>= lqipImages imageMetaData
                  >>= relativizeUrls


    -- ~ Normal updates
    matchMetadata "updates/**.md" draftCheck $ do
      route $ setExtension "html"
      tags <- buildTagsWith getTags "updates/**" (fromCapture "tags/*.html")

      let ctx =  constField "commit"  commitDetails
              <> bookContext

      compile $ getResourceBody
                  >>= renderPandoc
                  >>= loadAndApplyTemplate "templates/review.html"  ctx
                  >>= saveSnapshot "content"
                  >>= loadAndApplyTemplate "templates/default.html" ctx
                  >>= applyAsTemplate ctx
                  >>= lqipImages imageMetaData
                  >>= relativizeUrls

      tagsRules tags $ \tag pattern -> do
        route idRoute
        compile $ do
          books        <- recentFirst =<< loadAll pattern
          tagCloud     <- renderTagCloudWith makeLink (intercalate " ") 90 180 tags
          -- TODO: Load the shelf updates with the specific tags.
          -- shelfUpdates <- byIssueCreationTime =<< loadAll pattern
          let shelfUpdates = []

          let tagCtx =  constField "tag"        tag
                     <> listField  "books"      bookContext (return books)
                     <> listField  "shelfItems" bookContext (return shelfUpdates)
                     <> constField "tagCloud"   tagCloud
                     <> constField "commit"     commitDetails
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
        updates <- recentFirst =<< loadAll "updates/**"
        shelfUpdates <- byIssueCreationTime =<< loadAll "shelf/*.md"

        let ctx =  constField "commit"    commitDetails
                <> listField "books"      bookContext (return updates)
                <> listField "shelfItems" bookContext (return shelfUpdates)
                <> bbContext

        getResourceBody
          >>= applyAsTemplate ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= lqipImages imageMetaData
          >>= relativizeUrls

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = bbContext `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "updates/**.md" "content"
            renderAtom feedConf feedCtx posts


feedConf :: FeedConfiguration
feedConf = FeedConfiguration
    { feedTitle         = "Between Books"
    , feedDescription   = "The Interdependent Bookshop!"
    , feedAuthorName    = "Noon van der Silk"
    , feedAuthorEmail   = "noonsilk+-noonsilk@gmail.com"
    , feedRoot          = "https://betweenbooks.com.au/"
    }


byIssueCreationTime :: (MonadMetadata m) => [Item a] -> m [Item a]
byIssueCreationTime = liftM reverse . chronological'


-- TODO: Clean up these hacks.
chronological' :: (MonadMetadata m) => [Item a] -> m [Item a]
chronological' =
    sortByM $ getItemUTC' defaultTimeLocale . itemIdentifier
  where
    sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
    sortByM f xs = liftM (map fst . sortBy (comparing snd)) $
                   mapM (\x -> liftM (x,) (f x)) xs

getItemUTC' :: (MonadMetadata m)
            => TimeLocale        -- ^ Output time locale
            -> Identifier        -- ^ Input page
            -> m UTCTime         -- ^ Parsed UTCTime
getItemUTC' locale id' = do
    metadata <- getMetadata id'
    let tryField k fmt = lookupString k metadata >>= parseTime' fmt
    return $ fromMaybe (error "Bad date") (tryField "issueCreatedAt" "%Y-%m-%dT%H:%M:%SZ")
  where
    parseTime' = parseTimeM True locale


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
  <> mkAffiliate "affiliateLink"
    where
      -- Make the affiliate link from the bookshop link.
      mkAffiliate fieldName
        = field fieldName (\i -> do
            let identifier = itemIdentifier i
            metadata <- getMetadata identifier
            let link = maybe "" affiliateLink $ lookupString "bookshopLink" metadata
            return link
          )

      asList fieldName
        = listFieldWith fieldName bbContext (\i -> do
            let identifier = itemIdentifier i
            metadata <- getMetadata identifier
            let metas = maybe [] id $ lookupStringList fieldName metadata
            return $ map (\x -> Item (fromFilePath x) x) (sort metas)
          )


-- | Bookshop.org affiliate code.
affiliateCode :: String
affiliateCode = "7342"


affiliateLink :: String -> String
affiliateLink s =
  let items = splitOn "/" s
   in "https://uk.bookshop.org/a/" ++ affiliateCode ++ "/" ++ last items


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

