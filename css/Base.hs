{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE ConstraintKinds   #-}


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
import qualified Data.Text   as T
import qualified Clay.Filter as F
import qualified Clay.Pseudo as P
import qualified Clay.Media  as M


main :: IO ()
main = putCss css


allMargin n       = margin  n n n n
allPadding n      = padding n n n n
allBorderRadius n = borderRadius n n n n


margin0  = margin  (px 0) (px 0) (px 0) (px 0)
padding0 = padding (px 0) (px 0) (px 0) (px 0)


coreTextFont = fontFamily ["DM Sans"]   [serif]
monoFont     = fontFamily ["PT Mono"]   [monospace]
titleFont    = fontFamily ["Calistoga"] [serif]
menuFont     = fontFamily ["DM Sans"]   [serif]


-- |
standardFont =
  do 
     coreTextFont
     textAlign justify


-- | Header elements: (H1, H2, etc)
headerStuff =
  do
    h1 <> h2 ? do
      titleFont
      fontWeight normal

    h3 ? do
      menuFont
      fontStyle italic
      fontWeight normal
      marginTop (px 30)



bookLayout :: Css
bookLayout = do

  -- Big review

  div # "#book-review" ? do
    display       flex
    flexDirection row

  div # ".book-info" ? do
    menuFont
    display       flex
    allPadding    (px 20)
    background    white
    flexDirection column
    minWidth      (px 200)
    maxWidth      (px 300)
    img ? do
      maxHeight (pct 100)
      maxWidth  (pct 100)
      "object-fit" -: "cover"

  div # ".large-review" ? do
    marginLeft  (px 20)
    marginRight (px 20)
    maxWidth    (px 1000)

  div # ".summary" <> div # ".review" ? do
    marginLeft  (px 10)
    paddingLeft (px 10)


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

    

  -- Homepage
  div # ".books" ? do
    display       flex
    flexDirection row
    flexWrap      (FlexWrap "wrap")

    div # ".book" ? do
      background white
      allPadding (px 20)
      allMargin  (px 10)
      width      (px 600)
      display       flex
      flexDirection row

      div # ".book-image" ? do
        marginRight (px 10)
        img ? do
          maxWidth (px 200)

      div # ".date" ? do
        menuFont

      div # ".book-title" ? do
        marginBottom (px 5)
        a ? do
          titleFont
          fontSize (px 20)





css :: Css
css = do
  headerStuff


  let fontSelectors 
        = p 
        <> li
        <> blockquote
        <> a
        <> small



  fontSelectors ? do
    standardFont

  p <> li ? do
    fontSize   (px 14)
    lineHeight (px 26)

  body ? do
    fontSize (px 14)
    margin0
    padding0
    backgroundColor white

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

  a ? do
    color black
    visited & do
      color black
    hover & do
      background ("#ffd381" :: Color)


  div # "#content" ? do
    backgroundColor oldlace
    marginTop    (px 10)
    marginBottom (px 10)
    allPadding   (px 30)
    display      flex
    flexDirection column


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


  footer ? do
    marginTop    (px 20)
    marginBottom (px 20)
