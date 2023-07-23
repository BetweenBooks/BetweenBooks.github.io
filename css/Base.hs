{-# language OverloadedStrings #-}

-- http://fvisser.nl/clay/
import Clay
import Control.Monad  ( forM_ )
import Prelude hiding ( span
                      , div
                      , rem
                      )
import qualified Data.Text      as Text
import qualified Clay.Media     as Media

--------------------------------------------------------------------

allMargin n       = margin  n n n n
allPadding n      = padding n n n n
allBorderRadius n = borderRadius n n n n

margin0  = margin  (px 0) (px 0) (px 0) (px 0)
padding0 = padding (px 0) (px 0) (px 0) (px 0)

coreTextFont = fontFamily ["Noto Serif JP"] [serif]
monoFont     = fontFamily ["PT Mono"]       [monospace]
titleFont    = fontFamily ["Calistoga"]     [serif]
menuFont     = coreTextFont

--------------------------------------------------------------------

main :: IO ()
main = putCss css'

css' :: Css
css'
  =  basics
  >> logo
  >> headings
  >> fonts
  >> links
  >> mainHeader
  >> mainContent
  >> reviewPage
  >> bookList
  >> tags
  >> footerDecoration

basics :: Css
basics = do
  body ? do
    margin0
    padding0
    backgroundColor white

  let selectionStyle =
        do
          background mediumslateblue
          color      white

  selection          & selectionStyle
  "::-moz-selection" & selectionStyle

footerDecoration :: Css
footerDecoration = do
  let decoration =
        do
          background  silver
          height      (px 0.5)
          display     inlineBlock
          position    relative
          width       (pct 30)
          marginLeft  (px 20)
          marginRight (px 20)
          "bottom"    -: "-0.2rem"
          "content"   -: "''"

  div # ".decoration" ? do
    color silver
    display        flex
    alignItems     center
    justifyContent center

    before & decoration
    after  & decoration

  footer ? do
    marginTop    (px 20)
    marginBottom (px 20)

logo :: Css
logo = do
  li # "#logo" ? do
    a ? hover & background white

tags :: Css
tags = do
  (span #".tags" <> div #".tagCloud" ) ? do
    fontSize   (em 0.85)
    lineHeight (em 1.7)

  a #".tag" ? do
    monoFont
    textDecoration none
    allPadding (px 2)

  let mkTag t c = a # t ? borderBottom (px 2) solid c

  mkTag ".activism" aquamarine
  mkTag ".adventurous" sandybrown
  mkTag ".architecture" yellowgreen
  mkTag ".books" lavender
  mkTag ".buddhism" blueviolet
  mkTag ".climate" honeydew
  mkTag ".compassion" skyblue
  mkTag ".democracy" thistle
  mkTag ".economics" khaki
  mkTag ".empathy" slateblue
  mkTag ".ethics" wheat
  mkTag ".fantasy" thistle
  mkTag ".feminism" salmon
  mkTag ".fiction" rosybrown
  mkTag ".fun" orange
  mkTag ".history" peru
  mkTag ".informative" lightskyblue
  mkTag ".language" saddlebrown
  mkTag ".long-read" plum
  mkTag ".music" hotpink
  mkTag ".non-fiction" lightsteelblue
  mkTag ".parenting" sienna
  mkTag ".personal" pink
  mkTag ".philanthropy" lightblue
  mkTag ".philosophical" gold
  mkTag ".poetry" orchid
  mkTag ".politics" lightsalmon
  mkTag ".quirky" violet
  mkTag ".racism" chocolate
  mkTag ".short-read" lightslategray
  mkTag ".technology" coral
  mkTag ".traumatic" mediumblue
  mkTag ".travel" limegreen
  mkTag ".urban-planning" lightseagreen

  div #".tagCloud" ? do
    fontSize (em 1.1)
    marginLeft (px 20)
    marginRight (px 20)
    a ? do
      allMargin (px 5)
      lineHeight (em 2)

mainContent :: Css
mainContent = do
  div # "#content" ? do
    backgroundColor oldlace
    marginBottom (px 10)
    allPadding   (px 30)
    display      flex
    flexDirection column

bookList :: Css
bookList = do

  div # ".book" ? do
    display flex
    flexGrow 1
    flexBasis (pct 42)
    allPadding (pct 2)
    borderBottom (px 3) solid black

  section # ".books" ? do
    display flex
    flexWrap (FlexWrap "wrap")

  div # ".book-content" ? do
    display flex
    paddingLeft (px 20)
    flexDirection column


  ".book-image" ? do
    display flex
    alignItems center
    flexDirection column
    small ? do
      marginTop (px 10)
    img ? do
      width (px 200)

  -- Generate the cool shelves.
  forM_ [ Text.pack . show $ (2*w + 1) | w <- [0..200]] $ \n -> do
    div # ".book" # nthChild n ? do
      borderRight (px 3) solid black

  query Clay.all [Media.maxWidth 1200] $ do
    -- body ?
    --   fontSize (px 28)

    div # ".book-content" ? do
      marginRight (px 20)

    -- Other side of the shelf
    forM_ [ Text.pack . show $ (2*w) | w <- [0..200]] $ \n -> do
      div # ".book" # nthChild n ? do
        borderLeft (px 3) solid black
    --
    div # ".book" ? do
      flexBasis (pct 90)

    div # ".book-info" ? do
      maxWidth (pct 100)
      flexDirection row

    div # ".fact" ? do
      marginLeft (px 20)

    "#book-review" ? do
      flexDirection column

    div # ".large-review" ? do
      marginLeft (px 0)

  -- Smallest
  query Clay.all [Media.maxWidth 800] $ do
    div # "#content" ? do
      allMargin (px 5)
      allPadding (px 0)

    div # ".book" ? do
      flexDirection column
      paddingTop    (px 20)
      paddingBottom (px 20)

    ul # "#menu" ? do
      flexDirection column

    div # ".book-image" ? do
      width (pct 100)
      alignItems center

    ".book-image" ? img ? do
      width (pct 100)
      maxHeight (px 200)
      "object-fit" -: "cover"

bookList' :: Css
bookList' = do
  div # ".books" ? do
    div # ".draft" ? do
      background lightcyan

links :: Css
links = do
  a ? do
    color black
    visited & do
      color black
    hover & do
      background ("#ffd381" :: Color)

fonts :: Css
fonts = do
  body ? do
    fontSize (px 14)

  let fontSelectors = p <> li <> blockquote <> a <> small

  fontSelectors ? do
    coreTextFont
    textAlign justify

  p <> li ? do
    lineHeight (em 1.8)

  h1 ? small ? do
    fontSize (em 0.7)
    color    (grey)

  div # ".book-title" ? do
    marginBottom (px 3)
    a ? do
      titleFont
      fontSize (em 1.2)

  div # ".date" ? do
    color dimgrey
    menuFont

  div # ".book-authors" ? do
    menuFont

mainHeader :: Css
mainHeader = do
  "#header" ? do
    marginTop  (px 100)
    ul # "#menu" ? do
      margin0
      padding0
      marginLeft     (px 20)
      display        flex
      alignItems     center
      listStyleType  none
      justifyContent flexStart

    li ? do
      firstChild & do
        marginRight (px 50)
      padding0
      a ? do
        menuFont
        display block
        allMargin (px 12)

headings :: Css
headings = do
  h1 <> h2 ? do
    titleFont
    fontWeight normal

  h3 ? do
    menuFont
    fontStyle italic
    fontWeight normal
    marginTop (px 30)

  h4 ? do
    menuFont

reviewPage :: Css
reviewPage = do
  "#book-review" ? do
    display       flex
    flexDirection row

  ul # ".book-links" ? do
    margin0
    li ? do
      display inline
      margin0

  div # ".book-info" ? do
    menuFont
    display         flex
    allPadding      (px 20)
    flexDirection   column
    backgroundColor white
    minWidth        (px 200)
    maxWidth        (px 300)
    borderBottom    (px 3) solid black
    img ? do
      maxHeight (pct 100)
      maxWidth  (pct 100)
      marginBottom (px 10)
      "object-fit" -: "cover"

  div # ".large-review" ? do
    marginLeft  (px 20)
    marginRight (px 20)
    maxWidth    (px 1000)
    span # ".ch" ? do
      monoFont
      fontStyle normal
      fontSize (em 0.9)
      backgroundColor ("#eaeaea" :: Color)

  div # ".summary" <> div # ".review" ? do
    marginLeft  (px 10)
    paddingLeft (px 10)

    blockquote ? do
      paddingLeft (px 10)
      borderLeft  (px 1) solid silver
      fontStyle   italic

  div # ".book-info" ? do
    display flex
    flexDirection column

  div # ".fact" ? do
    marginTop     (px 5)
    marginBottom  (px 5)
    display       flex
    flexDirection row

    span # ".property" ? do
      color gray
      paddingRight (px 5)
