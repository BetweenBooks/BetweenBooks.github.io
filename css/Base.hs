{-# language OverloadedStrings #-}

-- http://fvisser.nl/clay/
import Clay
import Control.Monad  ( forM_ )
import Prelude hiding ( span
                      , div
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
    fontSize (px 14)
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
  span #".tags" ? do
    fontSize (px 11)
    monoFont
    lineHeight (px 22)

    a ? do textDecoration none

    a #".tag" ? do
      allPadding (px 2)

    let mkTag t c = a # t ? borderBottom solid (px 2) c

    mkTag ".travel" limegreen
    mkTag ".personal" pink
    mkTag ".quirky" violet
    mkTag ".informative" lightskyblue
    mkTag ".adventurous" sandybrown
    mkTag ".short-read" lightslategray
    mkTag ".philosophical" gold
    mkTag ".feminism" salmon
    mkTag ".non-fiction" lightsteelblue
    mkTag ".fun" orange
    mkTag ".fiction" rosybrown
    mkTag ".history" peru
    mkTag ".traumatic" mediumblue
    mkTag ".urban-planning" lightseagreen
    mkTag ".economics" khaki
    mkTag ".buddhism" blueviolet
    mkTag ".long-read" plum
    mkTag ".democracy" thistle
    mkTag ".ethics" wheat
    mkTag ".compassion" skyblue
    mkTag ".empathy" slateblue

mainContent :: Css
mainContent = do
  div # "#content" ? do
    backgroundColor oldlace
    marginTop    (px 10)
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
    borderBottom solid (px 3) black

  ".books" ? do
    display flex
    flexWrap (FlexWrap "wrap")

  div # ".book-content" ? do
    display flex
    paddingLeft (px 20)
    flexDirection column

  ".book-image" ? img ? do
    width (px 200)

  -- Generate the cool shelves.
  forM_ [ Text.pack . show $ (2*w + 1) | w <- [0..200]] $ \n -> do
    div # ".book" # nthChild n ? do
      borderRight solid (px 3) black
  
  query Clay.all [Media.maxWidth 1200] $ do
    -- Other side of the shelf
    forM_ [ Text.pack . show $ (2*w) | w <- [0..200]] $ \n -> do
      div # ".book" # nthChild n ? do
        borderLeft solid (px 3) black
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
  query Clay.all [Media.maxWidth 600] $ do
    div # ".book" ? do
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
  let fontSelectors = p <> li <> blockquote <> a <> small

  fontSelectors ? do
    coreTextFont
    textAlign justify

  p <> li ? do
    fontSize   (px 14)
    lineHeight (px 26)

  h1 ? small ? do
    fontSize (px 16)
    color    (grey)

  div # ".book-title" ? do
    marginBottom (px 3)
    a ? do
      titleFont
      fontSize (px 20)

  div # ".date" ? do
    color dimgrey
    menuFont

  div # ".book-authors" ? do
    menuFont

mainHeader :: Css
mainHeader = do
  header ? do
    marginTop (px 100)
    ul ? do
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
    borderBottom    solid (px 3) black
    img ? do
      maxHeight (pct 100)
      maxWidth  (pct 100)
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
      borderLeft  solid (px 1) silver
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
