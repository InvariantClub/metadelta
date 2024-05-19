{-# language OverloadedStrings   #-}
{-# language ImportQualifiedPost #-}

import Clay
import Clay.Media    qualified as Media
import Clay.Flexbox  qualified as Flex
import Clay.Elements qualified as Elts
import Control.Monad (forM_)
import Data.Text qualified as Text
import Prelude hiding (span, div, rem)

-- ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
--
-- Helpful things

allMargin n       = margin  n n n n
allPadding n      = padding n n n n
allBorderRadius n = borderRadius n n n n

margin0  = margin  (px 0) (px 0) (px 0) (px 0)
padding0 = padding (px 0) (px 0) (px 0) (px 0)

coreTextFont = fontFamily ["Open Sans"] [sansSerif]
monoFont     = fontFamily ["Fira Code"] [monospace]
titleFont    = fontFamily ["Libre Baskerville"] [sansSerif]
menuFont     = coreTextFont

bgColour :: Color
bgColour = "#fffbf0"

strongBgColour :: Color
strongBgColour = "#ffffff"

selectionColour :: Color
selectionColour = "#eaeaea"

strongHighlightColour :: Color
strongHighlightColour = "#e6e6fa"

textColour :: Color
textColour = "#383838"

softTextColour :: Color
softTextColour = "#787878"

linkColour :: Color
-- linkColour = "#9a99dd"
-- linkColour = "#b84db2"
linkColour = darken 0.2 otherStrongColour

otherStrongColour :: Color
otherStrongColour = "#6232ff"

linkSoftColour :: Color
linkSoftColour = "#cfceff"

linkSoftColour2 :: Color
linkSoftColour2 = "#efefef"

commentColour :: Color
commentColour = otherStrongColour

plainColour :: Color
plainColour = "#ffffff"

softBgColour :: Color
softBgColour = "#f0f0f0"

almostSoftBgColour :: Color
almostSoftBgColour = "#f5f5f5"


standardShadow = "box-shadow" -: "2.1px 4.3px 4.3px hsl(0deg 0% 0% / 0.25);"


replaced = "#dd99dc"
removed  = "#dd9a99"
added    = "#99dd9a"

withSmallDisplay = query Clay.all [Media.maxWidth 1200]
withPhoneDisplay = query Clay.all [Media.maxWidth  450]

--
-- ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


main :: IO ()
main = putCss
  $  basics
  >> fonts
  >> headings
  >> pageLayout
  >> legend'
  >> specialHighlights
  >> permissions
  >> groupings
  >> diffStyles
  >> statusBox
  >> footer'
  >> wiggle
  >> filteringWidget
  >> groupingWidget
  >> pasteWidget
  >> buttons
  >> infoStuff
  >> options
  >> loading
  >> lilGui


lilGui :: Css
lilGui = do
  ".lil-gui" ? do
    "--background-color"       -: "#f5f5f5"
    "--text-color"             -: "#383838"
    "--title-background-color" -: "#efefef"
    "--title-text-color"       -: "#383838"
    "--font-family-mono"       -: "Fira Code"
    "--font-size"              -: "18px"
    "--input-font-size"        -: "18px"
    "--widget-color"           -: "#ffffff"
    "--focus-color"            -: "#fefefe"
    "--hover-color"            -: "#ffffff"
    "--string-color"           -: "#383838"
    "--padding" -: "10px"
    "--spacing" -: "15px"


loading :: Css
loading = do
  div # ".loading.hidden" ? do
    display none

  div # ".loading" ? do
    display flex
    span # ".loader" ? do
      height $ px 4
      width $ pct 100
      display inlineBlock
      position relative
      overflow hidden
      after & do
        content $ stringContent ""
        width $ px 300
        height $ px 4
        backgroundColor linkSoftColour
        position absolute
        top $ px 0
        left $ px 0
        boxSizing borderBox
        "animation" -: "loader 8s linear infinite";

  keyframes "loader"
    [ (0,   do
        left $ pct 0
        "transform" -: "translateX(-100%)"
      )
    , (100, do
        left $ pct 100
        "transform" -: "translateX(0%)"
      )
    ]


infoStuff :: Css
infoStuff = do
  span # ".src" ? do
    backgroundColor white
    border (px 1) solid ("#eaeaea" :: Color)
    allBorderRadius $ px 10
    allPadding $ px 5
    paddingLeft $ px 0
    monoFont
    fontSize $ em 0.8

  span # ".src.solo" ? do
    paddingLeft $ px 5

  span # ".src.old" ? do
    before & do
      content (stringContent "old:")
      borderRadius (px 10) 0 0 (px 10)
      backgroundColor (lighten 0.5 removed)
      allPadding $ px 5
      paddingLeft $ px 8
      paddingRight $ px 8
      marginRight $ px 5

  span # ".src.new" ? do
    before & do
      content (stringContent "new:")
      borderRadius (px 10) 0 0 (px 10)
      backgroundColor (lighten 0.5 added)
      allPadding $ px 5
      paddingLeft $ px 8
      paddingRight $ px 8
      marginRight $ px 5

  ul # ".diff-info" ? do
    monoFont
    fontSize $ em 0.9
    Elts.em ? do
      fontStyle normal


pasteWidget :: Css
pasteWidget = do
  div # ".paste" ? do
    marginTop (px 20)
    marginLeft (px 30)
    width $ pct 80
    textarea ? do
      monoFont
      width      $ pct 100
      height     $ px 600
      allPadding $ px 10
      fontSize   $ em 0.8

    span # ".valid" ? do
      backgroundColor $ lighten 0.3 added
      allPadding      $ px 5
      paddingLeft     $ px 8
      paddingRight    $ px 8
      allBorderRadius $ px 5

    span # ".invalid" ? do
      backgroundColor $ lighten 0.3 removed
      allPadding      $ px 5
      paddingLeft     $ px 8
      paddingRight    $ px 8
      allBorderRadius $ px 5

    pre ? do
      width    $ pct 80
      fontSize $ em 0.8

    div # ".buttons" ? do
      display flex
      justifyContent spaceBetween
      alignContent center
      alignItems center


groupingWidget :: Css
groupingWidget = do
  div # ".diff-groups" ? do
    marginTop $ px 20

  div # "#groupingWidget" ? do
    flexWrap Flex.wrap
    display flex
    flexDirection row
    div # ".column" ? do
      alignItems center
      display flex
      flexDirection row
      paddingTop (px 0)



buttons :: Css
buttons = do
  button # ".selected" ? do
    fontWeight bold
    textDecoration underline
    disabled & do
      fontWeight normal
      textDecoration none

  button ? do
    coreTextFont
    allPadding      $ em 0.3
    paddingLeft     $ em 0.45
    paddingRight    $ em 0.45
    allMargin       $ em 0.4
    fontSize        $ px 20
    allBorderRadius $ px 10
    border (px 0) solid black
    cursor pointer
    standardShadow

    active & do
      transform $ translate (px 2) (px 2)
      disabled & do
        transform none

    disabled & do
      backgroundColor (rgb 230 230 230)

    hover & do
      backgroundColor linkSoftColour
      disabled & do
        backgroundColor (rgb 230 230 230)
        cursor notAllowed


filteringWidget :: Css
filteringWidget = do
  div # ".regexes" ? do
    display flex
    flexDirection column

    div # ".regex" ? do
      display flex
      flexDirection row
      alignItems center
      paddingTop    $ px 5
      paddingBottom $ px 5

    label ? do
      width (px 200)

    input ? do
      monoFont
      width      $ px 350
      allPadding $ px 2
      fontSize   $ px 20


options :: Css
options = do
  span # ".tt" ? do
    monoFont

  div # ".option" ? do
    display flex
    flexDirection row
    withSmallDisplay $ do
      flexDirection column

    alignItems flexStart
    marginTop $ px 20
    width $ pct 90

    span # ".name" ? do
      minWidth $ px 300

    input # ".token" ? do
      monoFont
      width       $ px 350
      allPadding  $ px 4
      fontSize    $ px 18
      minWidth    $ px 500

      withSmallDisplay $ do
        width (pct 90)
        minWidth (pct 90)

    div # ".inputs" ? do
      display flex
      flexDirection column
      marginLeft $ px 10

    div # ".help" ? do
      marginTop $ px 10
      maxWidth  $ px 1000
      fontSize  $ em 0.9
      color softTextColour

    p ? do
      marginBottom $ px 10



wiggle :: Css
wiggle = do
  keyframes "wiggle"
    [ (0,   "transform" -: "rotate(0deg)")
    , (5,   "transform" -: "rotate(3deg)")
    , (10,  "transform" -: "rotate(-3deg)")
    , (15,  "transform" -: "rotate(0deg)")
    , (100, "transform" -: "rotate(0deg)")
    ]


footer' :: Css
footer' = do
  div # ".footer" ? do
    alignItems center
    color softTextColour
    display flex
    flexDirection column
    fontSize $ em 0.8
    justifyContent center
    marginTop $ px 100
    marginBottom $ px 100


statusBox :: Css
statusBox = do
  div # ".status.wiggle" ? do
    "animation" -: "wiggle 1.5s 1";
    backgroundColor (lighten 0.8 added)
    div # ".progress" ? do
      span # ".progress" ? do
          backgroundColor added

  div # ".status" ? do
    position fixed
    bottom $ em 2
    left  $ em 2
    "width" -: "calc(500px + 3ch)"
    display flex
    monoFont
    flexDirection column
    alignItems center
    fontSize (em 0.7)
    backgroundColor softBgColour
    standardShadow
    justifyContent spaceBetween

    withPhoneDisplay $ do
      "width" -: "calc(200px + 3ch)"

    div # ".text" ? do
      allPadding (px 5)

    div # ".progress" ? do
      display flex
      width (pct 100)
      height (px 4)
      allBorderRadius (px 5)
      backgroundColor softBgColour
      span # ".progress" ? do
        backgroundColor linkColour


diffStyles :: Css
diffStyles = do

  div # ".indicator" ? do
    width (px 20)
    borderRadius (px 20) 0 0 (px 20)

  div # ".diff-item-removed" ? do
     backgroundImage (url "images/removed.svg")

  span # ".diff-item-removed" ? do
     backgroundColor (lighten 0.6 removed)
     hover & backgroundColor (lighten 0.7 removed)

  div # ".diff-item-added" ? do
    backgroundImage (url "images/added.svg")

  span # ".diff-item-added" ? do
     backgroundColor (lighten 0.5 added)
     hover & backgroundColor (lighten 0.6 added)

  div # ".operation-removed" ? do
    backgroundImage (url "images/removed-weak.svg")
    border (px 1) solid removed
    allBorderRadius $ px 5


  div # ".operation-added" ? do
    backgroundImage (url "images/added-weak.svg")
    border (px 1) solid added
    allBorderRadius $ px 5


groupings :: Css
groupings = do
  a # ".anchor" ? do
    textDecoration none
    fontWeight normal
    color black
    paddingRight $ px 5
    color softTextColour
    hover & backgroundColor bgColour

  span # ".group-header" ? do
     lineHeight  $ em 2
     marginLeft  $ em 0.3
     marginRight $ em 0.3
     allPadding  $ em 0.2
     withPhoneDisplay $ allMargin (px 0)

  div # ".level-1" ? do
    borderLeft (px 5) solid (rgb 230 230 230)
    allPadding      $ em 1
    allMargin       $ em 1
    allBorderRadius $ em 0.5
    background (rgb 250 250 250)
    standardShadow
    withPhoneDisplay $ do
      allMargin  (px 0)
      allPadding (px 5)

    small # ".label" ? do
      color softTextColour
      textTransform lowercase

  div # ".level-2" ? do
     allMargin $ em 0.5
     withPhoneDisplay $ allMargin (px 0)


permissions :: Css
permissions = do
  div # ".all-permissions" ? do
    display flex
    flexDirection column
  div # ".permission-item" ? do
    display flex
    flexDirection row
    alignItems center
    paddingLeft  $ px 10
    marginTop    $ em 0.6
    marginBottom $ em 0.6

    withSmallDisplay $ do
      flexDirection column
      maxWidth (pct 100)
      alignItems flexStart

    withPhoneDisplay $ allMargin (px 0)

  div # ".table" <> div # ".role" ? do
     fontSize (em 1.1)

  span # ".prefix" ? do
     color softTextColour
     width    $ px 200
     minWidth $ px 200
     span # ".actual-text" ? do
      allPadding $ px 4
      allBorderRadius $ px 10
      backgroundColor softBgColour


  pre # ".row-filter" ? do
     monoFont
     allPadding (em 0.4)
     paddingRight (em 2)
     backgroundColor (lighten 0.3 bgColour)
     allBorderRadius (px 10)
     fontSize (em 0.8)
     overflow auto
     "box-shadow" -: "2.1px 4.3px 4.3px hsl(0deg 0% 0% / 0.25);"
     whiteSpace preWrap
     -- TODO: Resolve

  div # ".permission-content" ? do
    display flex
    flexDirection row
    alignItems center
    allPadding (px 10)
    flexGrow 2
    withSmallDisplay $ do
      flexDirection column
      alignItems flexStart

    withPhoneDisplay $ do
      allPadding $ px 0
      allMargin  $ px 0

    div # ".cdots" ? do
      display none

    div # ".cdots.viewed" ? do
      display flex
      marginLeft (px 10)

    div # ".content.viewed" ? do
      display none

  div # ".permission-atom" ? do
    backgroundColor softBgColour
    marginTop     $ em 1
    marginBottom $ em 1
    allBorderRadius (px 20)
    display flex
    "box-shadow" -: "3px 3px 16px 0px rgba(154,153,221,0)"
    withSmallDisplay $ do
      flexDirection column

  div # ".permission-atom.changed" ? do
    "box-shadow" -: "3px 3px 16px 0px rgba(154,153,221,0.7)"

  -- Don't do things on hover.
  -- div # ".permission-atom" # hover ? do
  --   backgroundColor bgColour
  --   span # ".actual-text" ? do
  --     backgroundColor bgColour


  div # ".tick-box" ? do
    display flex
    alignItems center
    justifyContent center
    marginRight (em 2)
    input ? do
      transform  $ scale 1.3 1.3

    label ? do
      allPadding (em 0.5)

  -- TODO: There's a bit of a css quirk that these are occasionally embedded
  -- in each other; it's solved in a quirky way for now, but so be it.
  div # ".operation" ? do
    display flex
    flexDirection column
    paddingLeft     $ px 0
    marginTop       $ em 0.3
    marginBottom    $ em 0.3
    allBorderRadius $ px 8

  div # ".op" ? do
    display flex
    flexDirection row
    flexWrap Flex.wrap
    alignItems center
    fontSize $ em 0.95

    withSmallDisplay $ do
      marginTop $ px 10

    span # ".Select" ? do
      backgroundColor (rgb 255 255 224)

    span # ".Update" ? do
      backgroundColor (rgb 240 248 255)

    span # ".Insert" ? do
      backgroundColor (rgb 240 255 240)

    span # ".Delete" ? do
      backgroundColor (rgb 255 228 225)

    span # ".op" ? do
      standardShadow
      monoFont
      allBorderRadius $ px 10
      fontSize        $ em 0.9
      marginRight     $ em 0.5
      allPadding      $ em 0.4
      textTransform uppercase

  div # ".sql-columns" ? do
    display flex
    flexWrap Flex.wrap
    flexDirection row
    alignItems center

  span # ".sql-column" ? do
    allPadding  $ em 0.3
    allMargin   $ em 0.7
    borderBottom (px 1) solid black
    monoFont
    fontSize $ em 0.9

  span # ".simple-text" ? do
     allPadding  $ em 0.3
     marginLeft  $ em 0.6
     marginRight $ em 0.6



specialHighlights :: Css
specialHighlights = do
  span # ".highlight-num" ? do
    allPadding (em 0.5)
    allBorderRadius (em 0.5)
    backgroundColor strongHighlightColour
    "box-shadow" -: "2.1px 4.3px 4.3px hsl(0deg 0% 0% / 0.25);"


  span # ".id-num" ? do
    monoFont
    border (px 1) solid softBgColour
    allBorderRadius $ px 30
    fontSize        $ em 0.8
    allPadding      $ px 5
    paddingLeft     $ px 12
    paddingRight    $ px 12
    position relative

  span # ".id-num.changed" ? do
    backgroundColor plainColour
    border (px 1) solid strongHighlightColour


legend' :: Css
legend' = do
  div # ".legend" ? do
    display flex
    color softTextColour
    fontSize $ em 0.8
    flexDirection column

    div # ".legend-items" ? do
      display flex
      flexDirection row
      justifyContent spaceEvenly
      alignItems center
      flexWrap Flex.wrap
      alignContent spaceBetween
      "gap" -: "30px"
      marginLeft  $ px 20
      marginRight $ px 20

      div # ".i" ? do
        display flex
        alignItems center
        span # ".id-num" ? do
          fontSize    $ em 1
          left        $ px 0
          marginRight $ px 4
          position static

      div # ".operation-added" <> div # ".operation-removed" ?
        allPadding (px 5)

      span # ".t" ? do
        allPadding (px 5)


  div # ".sp.operation" ? do
    backgroundColor (rgb 240 240 240)
    flexDirection row
    borderRadius (px 20) 0 0 (px 20)


pageLayout :: Css
pageLayout = do
  div # ".most-things" ? do
    display flex
    flexDirection column

  div # ".most-things.hidden" ? do
    display none

  div # ".banner" ? do
    backgroundColor bgColour
    allPadding    $ px 20
    paddingBottom $ px 0

    div # ".links" ? do
      paddingTop    $ px 10
      display flex
      justifyContent spaceBetween
      div # ".pages" ? do
        display flex
        span ? do
          marginLeft  $ px 5
          marginRight $ px 5
      span # ".active" ? do
        backgroundColor white
      span ? do
        borderTopLeftRadius  (px 5) (px 5)
        borderTopRightRadius (px 5) (px 5)
        backgroundColor bgColour
        allPadding   $ px 10
        paddingLeft  $ px 10
        paddingRight $ px 10
        fontSize     $ em 0.85

  div # ".content" ? do
    marginLeft (px 40)
    display flex
    flexDirection column
    marginRight $ px 40
    withPhoneDisplay $ do
      allMargin $ px 5


  div # ".error" ? do
    b ? do
      marginBottom $ px 20
    paddingLeft $ px 10
    borderLeft (px 5) solid removed


headings :: Css
headings = do
  h1 ? do
    margin0
    titleFont

  h2 <> h3 <> h4 <> h5 ? do
    margin0

  h1 ? do
    fontStyle italic
    a ? do
      color black
      textDecoration none

    span # ".beta" ? do
      allBorderRadius (px 5)
      allMargin (px 8)
      allPadding (px 2)
      background linkColour
      color white
      fontSize (em 0.4)
      monoFont
      paddingLeft (px 5)
      paddingRight (px 5)
      "box-shadow" -: "3.7px 4.3px 4.3px hsl(0deg 0% 0% / 0.15);"

  h2 ? do
    fontSize (em 1.3)
    -- span ? do
    --   textDecoration underline

  h3 ? do
    allMargin $ px 30
    withPhoneDisplay $ do
      allMargin  $ px 10
      marginLeft $ px 5


  div # ".level-2" |> h3 ? do
    margin0

  h4 ? do
    marginTop (px 20)
    coreTextFont


fonts :: Css
fonts = do
  body ? do
    coreTextFont
    fontSize (px 22)
    color textColour
    -- backgroundColor bgColour
    withPhoneDisplay $ do
      fontSize (px 18)


basics :: Css
basics = do
  let elts
        =  body
        <> section
        <> footer
        <> nav

  elts ? do
    display flex
    flexDirection column
    margin0
    padding0

  p <> li <> ul ? do
    margin0

  p <> li ? do
    lineHeight (px 40)

  a ? do
    color black

  a # hover ? do
    fontStyle italic
    -- backgroundColor linkSoftColour2
    color black

  let selectionStyle =
        do
          background selectionColour
          color      black

  selection          & selectionStyle
  "::-moz-selection" & selectionStyle
