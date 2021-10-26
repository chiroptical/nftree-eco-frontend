module Css.Input where

import Tailwind as T
import Web.HTML.Common (ClassName)

input :: Array ClassName
input =
  [ T.appearanceNone
  , T.roundedNone
  , T.relative
  , T.block
  , T.wFull
  , T.px3
  , T.py2
  , T.border
  , T.borderGray300
  , T.placeholderGray500
  , T.textGray900
  , T.roundedTMd
  , T.focusOutlineNone
  , T.focusRingIndigo500
  , T.focusBorderIndigo500
  , T.focusZ10
  , T.smTextSm
  ]

inputSubmit :: Array ClassName
inputSubmit =
  [ T.group
  , T.relative
  , T.wFull
  , T.flex
  , T.justifyCenter
  , T.py2
  , T.px4
  , T.border
  , T.borderTransparent
  , T.textSm
  , T.fontMedium
  , T.roundedMd
  , T.textWhite
  , T.bgIndigo600
  , T.hoverBgIndigo700
  , T.focusOutlineNone
  , T.focusRing2
  , T.focusRingOffset2
  , T.focusRingIndigo500
  ]
