{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}


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


coreTextFont = fontFamily ["PT Serif"]        [serif]
monoFont     = fontFamily ["PT Mono"]       [monospace]
titleFont    = fontFamily ["Calistoga"]     [serif]
menuFont     = fontFamily ["DM Mono"]  [serif]


-- |
standardFont =
  do 
     coreTextFont
     textAlign justify

-- | Header elements: (H1, H2, etc)
headerStuff =
  do
    h1 <> h2 <> h3 <> h4 <> h6 ? do
      titleFont
      fontWeight normal

    -- h1 ? do
    --   textAlign center
    --   fontSize  (rem 3)

    -- header ? do
    --   paddingBottom (px 40)

      -- marginBottom  (px 10)
--     h2 ? fontSize (rem 1.4)
--     h3 ? fontSize (rem 1.3)
--     h4 ? fontSize (rem 1.2)

--     h5 # "#manifold" ? do
--       monoFont
--       fontSize   (rem 1.2)
--       fontWeight normal

--     h1 ** small ? do
--       fontSize   (rem 1.1)
--       monoFont



css :: Css
css = do
  headerStuff

  body ? do
    fontSize (px 14)
    margin0
    padding0
    backgroundColor oldlace

  let fontSelectors = p 
        <> li
        <> blockquote

  fontSelectors ? do
    standardFont

  let selectionStyle =
        do
          background mediumslateblue
          color      white

  selection          & selectionStyle
  "::-moz-selection" & selectionStyle


--   section # ".content" ? do
--     display flex
--     flexDirection  column
--     justifyContent center
--     alignItems     center
--     div # ".content" ? do
--       width (px 900)

  -- header <> section <> footer ? do
  --   maxWidth (px 1000)
  --   backgroundColor white
    -- marginLeft  (px 20)
    -- marginRight (px 20)

  -- header # ".compressed" ? do
  --   width (pct 100)
  --   background lightgray
  --   height (px 100)
  --   fontSize (rem 0.5)
  --   h1 ? fontSize (rem 0.9)

  a ? do
    color royalblue
    textDecoration none
    hover & do
      -- background mediumslateblue
      -- color      white
      textDecoration underline

  div # ".container" ? do
    maxWidth    (px 980)
    marginLeft  auto
    marginRight auto

  div # ".page-container" ? do
    background    white
    borderTop     solid (px 1) white
    paddingBottom (px 20)

  div # "role=main" ? do
    clear both
    allMargin (px 20)
    after & do
      display block
      "content" -: "' '"
      height (px 0)
      clear both
      visibility hidden

  -- Compressed header
  let headerSmall =
        do
          "#big-logo"   ? do
            "transition"  -: "1s"
            display none

          "#small-logo" ? do
            "transition"  -: "1s"
            display block

          marginTop     (px 0)
          display       block
          img ? do
            "transition"  -: "1s"
            allPadding (px 10)
            height     (px 30)
            width      auto
          nav # "role=navigation" ? do
            "transition"  -: "1s"
            background   white
            borderBottom solid (px 1) silver
            height       (px 50)
            display      block
            after & do
              display block
              "content" -: "' '"
              height (px 0)
              clear both
              visibility hidden
            a ? do
              fontSize  (rem 0.8)
            ul ? do
              "transition"  -: "1s"
              margin0
              padding0
              listStyleType none
              justifyContent flexStart
              display       block
            li ? do
              "transition"  -: "1s"
              menuFont
              float    floatLeft
              padding0
              a ? do
                display block
                allMargin (px 12)
                padding (px 0) (px 0) (px 6) (px 0)

  let headerBig = 
        do
          "#small-logo" ? do
            display none

          marginTop      (px 20)
          display        flex
          justifyContent center
          img ? do
            "transition"  -: "1s"
            width (px 400)
          nav # "role=navigation" ? do
            "transition"  -: "1s"
            display        flex
            justifyContent center
            flexDirection  column
            ul ? do
              margin0
              marginTop    (px 20)
              marginBottom (px 20)
              padding0
              display        flex
              justifyContent center
              listStyleType  none
              flexDirection  row
              flexWrap       (FlexWrap "wrap")
              li ? do
                menuFont
                paddingLeft  (px 10)
                paddingRight (px 10)

  let features = [M.maxWidth (px 560)]

  query    M.screen features (header ? headerSmall)
  queryNot M.screen features (header ?   headerBig)

  header # ".small" ? headerSmall
  header # ".big"   ? headerBig


  -- div # "#shell" ? do
    -- allMargin       (px 10)
    -- marginTop       (px  0)
    -- paddingTop      (px  0)
    -- allPadding      (px 10)
    -- allBorderRadius (px  3)
    -- display        flex
    -- flexDirection  column
    -- alignItems     center
    -- justifyContent center

  let decoration =
        do
          background  silver
          height      (px 0.5)
          display     inlineBlock
          position    relative
          width       (pct 100)
          marginLeft  (px 20)
          marginRight (px 20)
          "bottom"    -: "-0.2rem"
          "content"   -: "''"

  div # ".decoration" ? do
    -- marginTop (px 20)
    color silver
    display        flex
    alignItems     center
    justifyContent center
    before & decoration
    after  & decoration

  -- ul # "#menu" ? do
  --   margin0
  --   padding0
  --   display        flex
  --   justifyContent center
  --   listStyleType  none
  --   flexDirection  row
  --   flexWrap       (FlexWrap "wrap")
  --   -- paddingLeft  (px 50)
  --   -- paddingRight (px 50)
  --   li ? do
  --     menuFont
  --     paddingLeft  (px 10)
  --     paddingRight (px 10)



