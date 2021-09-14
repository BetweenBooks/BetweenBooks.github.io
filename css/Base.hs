{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- http://fvisser.nl/clay/
import Clay
import Clay.Selector  ( refinementFromText )
import Control.Monad  ( forM_ )
import Prelude hiding ( span
                      , div
                      , (**)
                      , filter
                      , rem
                      )
import qualified Data.Text      as T
import qualified Clay.Filter    as F
import qualified Clay.Pseudo    as P
import qualified Clay.Media     as M
import qualified Clay.Elements  as E

main :: IO ()
main = putCss css'

css' :: Css
css'
  =  headings
  >> fonts
  >> links
  >> mainHeader
  >> mainContent
  >> tags 
  >> body'
  >> footer'

tags :: Css
tags = do
  span #".tags" ? do
    fontSize (px 11)
    monoFont

    a ? do
      textDecoration none

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
    mkTag ".urban" lightseagreen
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
  section # ".books" ? do
    display  flex
    flexWrap (FlexWrap "wrap")

  div # ".book" ? do
    display    flex
    flexGrow   1
    flexBasis  (pct 16)
    allPadding (pct 2)

    div # ".book-image" ? img ? do
      maxWidth (pct 100)

  div # ".book-content" ? do
    marginTop auto



bookList' :: Css
bookList' = do
  div # ".books" ? do
    display       flex
    flexDirection row
    flexWrap      (FlexWrap "wrap")

    div # ".book" ? do
      flexWrap      (FlexWrap "wrap")
      allPadding    (px 20)
      allMargin     (px 0)
      maxWidth      (px 400)
      display       flex
      flexDirection row
      borderBottom  solid (px 10) black

      div # ".book-image" ? do
        marginRight (px 10)
        img ? do 
          maxHeight (pct 100)
          minWidth (pct 100)
          "object-fit" -: "cover"
          verticalAlign vAlignBottom

      div # ".date" ? do
        menuFont


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
    marginBottom (px 5)
    a ? do
      titleFont
      fontSize (px 20)

  div # ".date" ? do
    color dimgrey

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

body' :: Css
body' = mempty

footer' :: Css
footer' = 
  footer ? do
    marginTop    (px 20)
    marginBottom (px 20)

-- header

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



bookLayout :: Css
bookLayout = do

  -- Big review
  --
  -- span # ".tags" ? do
  --   a ? do
  --     backgroundColor  cornsilk
  --     allMargin        (px 5)
  --     allPadding       (px 4)
  --     allBorderRadius  (px 5)

  div # "#book-review" ? do
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
    backgroundColor white
    flexDirection   column
    minWidth        (px 200)
    maxWidth        (px 300)
    border          solid (px 1) black
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


  div # ".book-authors" ? do
    monoFont
    textTransform uppercase

  div # ".fact" ? do
    marginTop     (px 5)
    marginBottom  (px 5)
    display       flex
    flexDirection row

    span # ".property" ? do
      color gray
      paddingRight (px 5)

    


css :: Css
css = do
  body ? do
    fontSize (px 14)
    margin0
    padding0
    backgroundColor white

  li # "#logo" ? do
    a ? do
      hover & do
        background white


  bookLayout


  let selectionStyle =
        do
          background mediumslateblue
          color      white

  selection          & selectionStyle
  "::-moz-selection" & selectionStyle



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


