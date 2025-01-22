{-# LANGUAGE OverloadedStrings #-}
module CSS where

import Clay
import Data.Text.Lazy (Text)
import qualified Clay.Render as R

-- | Main blog style
blogStyle :: Css
blogStyle = do
  -- | Global styles
  body ? do
    margin (px 0) (px 0) (px 0) (px 0)
    backgroundColor "#f5f5f5"
    fontFamily ["system-ui"] [sansSerif]
    lineHeight (px 1.6)
    color "#333"

  -- | Layout
  ".container" ? do
    maxWidth (px 800)
    margin (px 0) auto (px 0) auto
    padding (px 20) (px 20) (px 20) (px 20)

  -- | Navigation
  ".nav" ? do
    padding (px 20) (px 0) (px 20) (px 0)
    marginBottom (px 40)
    borderBottom (px 1) solid "#eee"
    a ? do
      marginRight (px 20)
      color "#333"
      textDecoration none
      fontWeight bold
      hover & do
        color "#007acc"

  -- | Blog post
  ".blog-post" ? do
    backgroundColor white
    padding (px 30) (px 30) (px 30) (px 30)
    marginBottom (px 40)
    borderRadius (px 8) (px 8) (px 8) (px 8)
    boxShadow $ pure $ bsColor (rgba 0 0 0 0.1) $ shadowWithBlur (px 0) (px 2) (px 4)

  -- | Headers
  h1 ? do
    fontSize (px 36)
    color "#333"
    marginBottom (px 20)
    fontWeight bold

  h2 ? do
    fontSize (px 28)
    color "#444"
    marginTop (px 30)
    marginBottom (px 15)

  -- | Content elements
  p ? do
    marginBottom (px 20)
    color "#555"
    lineHeight (px 1.8)

  img ? do
    maxWidth (pct 100)
    height auto
    display block
    margin (px 20) auto (px 20) auto
    borderRadius (px 4) (px 4) (px 4) (px 4)

  figcaption ? do
    textAlign center
    fontStyle italic
    color "#666"
    marginTop (px 8)

  -- | Code blocks
  pre ? do
    backgroundColor "#f8f8f8"
    padding (px 15) (px 15) (px 15) (px 15)
    borderRadius (px 4) (px 4) (px 4) (px 4)
    overflow auto
    code ? do
      fontFamily ["Consolas", "Monaco", "Andale Mono"] [monospace]
      fontSize (px 14)

  -- | Footer
  ".footer" ? do
    marginTop (px 60)
    padding (px 20) (px 0) (px 20) (px 0)
    borderTop (px 1) solid "#eee"
    textAlign center
    color "#666"

-- | Render CSS to Text
renderCSS :: Text
renderCSS = R.renderWith R.compact [] blogStyle 