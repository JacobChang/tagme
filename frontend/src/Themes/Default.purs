module Themes.Default where

import CSS
import CSS.Color as Color

type Theme =
  { primaryColor :: Color }

defaultTheme :: Theme
defaultTheme =
  { primaryColor: Color.rgb 70 230 125 }