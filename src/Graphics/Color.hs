module Graphics.Color
    ( maroon
    , darkRed
    , brown
    , firebrick
    , crimson
    , red
    , tomato
    , coral
    , indianRed
    , lightCoral
    , darkSalmon
    , salmon
    , lightSalmon
    , orangeRed
    , darkOrange
    , orange
    , gold
    , darkGoldenRod
    , goldenRod
    , paleGoldenRod
    , darkKhaki
    , khaki
    , olive
    , yellow
    , yellowGreen
    , darkOliveGreen
    , oliveDrab
    , lawnGreen
    , chartreuse
    , greenYellow
    , darkGreen
    , green
    , forestGreen
    , lime
    , limeGreen
    , lightGreen
    , paleGreen
    , darkSeaGreen
    , mediumSpringGreen
    , springGreen
    , seaGreen
    , mediumAquaMarine
    , mediumSeaGreen
    , lightSeaGreen
    , darkSlateGray
    , teal
    , darkCyan
    , aqua
    , cyan
    , lightCyan
    , darkTurquoise
    , turquoise
    , mediumTurquoise
    , paleTurquoise
    , aquaMarine
    , powderBlue
    , cadetBlue
    , steelBlue
    , cornFlowerBlue
    , deepSkyBlue
    , dodgerBlue
    , lightBlue
    , skyBlue
    , lightSkyBlue
    , midnightBlue
    , navy
    , darkBlue
    , mediumBlue
    , blue
    , royalBlue
    , blueViolet
    , indigo
    , darkSlateBlue
    , slateBlue
    , mediumSlateBlue
    , mediumPurple
    , darkMagenta
    , darkViolet
    , darkOrchid
    , mediumOrchid
    , purple
    , thistle
    , plum
    , violet
    , magenta, fuchsia
    , orchid
    , mediumVioletRed
    , paleVioletRed
    , deepPink
    , hotPink
    , lightPink
    , pink
    , antiqueWhite
    , beige
    , bisque
    , blanchedAlmond
    , wheat
    , cornSilk
    , lemonChiffon
    , lightGoldenRodYellow
    , lightYellow
    , saddleBrown
    , sienna
    , chocolate
    , peru
    , sandyBrown
    , burlyWood
    , tan
    , rosyBrown
    , moccasin
    , navajoWhite
    , peachPuff
    , mistyRose
    , lavenderBlush
    , linen
    , oldLace
    , papayaWhip
    , seaShell
    , mintCream
    , slateGray
    , lightSlateGray
    , lightSteelBlue
    , lavender
    , floralWhite
    , aliceBlue
    , ghostWhite
    , honeydew
    , ivory
    , azure
    , snow
    , black
    , dimGray, dimGrey
    , gray, grey
    , darkGray, darkGrey
    , silver
    , lightGray, lightGrey
    , gainsboro
    , whiteSmoke
    , white
    ) where

import Prelude hiding (tan)
import Linear

-- Forcing a float format for the literals is necessary when dealing with (V x Float) type.
-- TODO Find why.

maroon :: Floating a => V3 a
maroon = V3 128.0 0.0 0.0 / 255.0
darkRed :: Floating a => V3 a
darkRed = V3 139.0 0.0 0.0 / 255.0
brown :: Floating a => V3 a
brown = V3 165.0 42.0 42.0 / 255.0
firebrick :: Floating a => V3 a
firebrick = V3 178.0 34.0 34.0 / 255.0
crimson :: Floating a => V3 a
crimson = V3 220.0 20.0 60.0 / 255.0
red :: Floating a => V3 a
red = V3 255.0 0.0 0.0 / 255.0
tomato :: Floating a => V3 a
tomato = V3 255.0 99.0 71.0 / 255.0
coral :: Floating a => V3 a
coral = V3 255.0 127.0 80.0 / 255.0
indianRed :: Floating a => V3 a
indianRed = V3 205.0 92.0 92.0 / 255.0
lightCoral :: Floating a => V3 a
lightCoral = V3 240.0 128.0 128.0 / 255.0
darkSalmon :: Floating a => V3 a
darkSalmon = V3 233.0 150.0 122.0 / 255.0
salmon :: Floating a => V3 a
salmon = V3 250.0 128.0 114.0 / 255.0
lightSalmon :: Floating a => V3 a
lightSalmon = V3 255.0 160.0 122.0 / 255.0
orangeRed :: Floating a => V3 a
orangeRed = V3 255.0 69.0 0.0 / 255.0
darkOrange :: Floating a => V3 a
darkOrange = V3 255.0 140.0 0.0 / 255.0
orange :: Floating a => V3 a
orange = V3 255.0 165.0 0.0 / 255.0
gold :: Floating a => V3 a
gold = V3 255.0 215.0 0.0 / 255.0
darkGoldenRod :: Floating a => V3 a
darkGoldenRod = V3 184.0 134.0 11.0 / 255.0
goldenRod :: Floating a => V3 a
goldenRod = V3 218.0 165.0 32.0 / 255.0
paleGoldenRod :: Floating a => V3 a
paleGoldenRod = V3 238.0 232.0 170.0 / 255.0
darkKhaki :: Floating a => V3 a
darkKhaki = V3 189.0 183.0 107.0 / 255.0
khaki :: Floating a => V3 a
khaki = V3 240.0 230.0 140.0 / 255.0
olive :: Floating a => V3 a
olive = V3 128.0 128.0 0.0 / 255.0
yellow :: Floating a => V3 a
yellow = V3 255.0 255.0 0.0 / 255.0
yellowGreen :: Floating a => V3 a
yellowGreen = V3 154.0 205.0 50.0 / 255.0
darkOliveGreen :: Floating a => V3 a
darkOliveGreen = V3 85.0 107.0 47.0 / 255.0
oliveDrab :: Floating a => V3 a
oliveDrab = V3 107.0 142.0 35.0 / 255.0
lawnGreen :: Floating a => V3 a
lawnGreen = V3 124.0 252.0 0.0 / 255.0
chartreuse :: Floating a => V3 a
chartreuse = V3 127.0 255.0 0.0 / 255.0
greenYellow :: Floating a => V3 a
greenYellow = V3 173.0 255.0 47.0 / 255.0
darkGreen :: Floating a => V3 a
darkGreen = V3 0.0 100.0 0.0 / 255.0
green :: Floating a => V3 a
green = V3 0.0 128.0 0.0 / 255.0
forestGreen :: Floating a => V3 a
forestGreen = V3 34.0 139.0 34.0 / 255.0
lime :: Floating a => V3 a
lime = V3 0.0 255.0 0.0 / 255.0
limeGreen :: Floating a => V3 a
limeGreen = V3 50.0 205.0 50.0 / 255.0
lightGreen :: Floating a => V3 a
lightGreen = V3 144.0 238.0 144.0 / 255.0
paleGreen :: Floating a => V3 a
paleGreen = V3 152.0 251.0 152.0 / 255.0
darkSeaGreen :: Floating a => V3 a
darkSeaGreen = V3 143.0 188.0 143.0 / 255.0
mediumSpringGreen :: Floating a => V3 a
mediumSpringGreen = V3 0.0 250.0 154.0 / 255.0
springGreen :: Floating a => V3 a
springGreen = V3 0.0 255.0 127.0 / 255.0
seaGreen :: Floating a => V3 a
seaGreen = V3 46.0 139.0 87.0 / 255.0
mediumAquaMarine :: Floating a => V3 a
mediumAquaMarine = V3 102.0 205.0 170.0 / 255.0
mediumSeaGreen :: Floating a => V3 a
mediumSeaGreen = V3 60.0 179.0 113.0 / 255.0
lightSeaGreen :: Floating a => V3 a
lightSeaGreen = V3 32.0 178.0 170.0 / 255.0
darkSlateGray :: Floating a => V3 a
darkSlateGray = V3 47.0 79.0 79.0 / 255.0
teal :: Floating a => V3 a
teal = V3 0.0 128.0 128.0 / 255.0
darkCyan :: Floating a => V3 a
darkCyan = V3 0.0 139.0 139.0 / 255.0
aqua :: Floating a => V3 a
aqua = V3 0.0 255.0 255.0 / 255.0
cyan :: Floating a => V3 a
cyan = V3 0.0 255.0 255.0 / 255.0
lightCyan :: Floating a => V3 a
lightCyan = V3 224.0 255.0 255.0 / 255.0
darkTurquoise :: Floating a => V3 a
darkTurquoise = V3 0.0 206.0 209.0 / 255.0
turquoise :: Floating a => V3 a
turquoise = V3 64.0 224.0 208.0 / 255.0
mediumTurquoise :: Floating a => V3 a
mediumTurquoise = V3 72.0 209.0 204.0 / 255.0
paleTurquoise :: Floating a => V3 a
paleTurquoise = V3 175.0 238.0 238.0 / 255.0
aquaMarine :: Floating a => V3 a
aquaMarine = V3 127.0 255.0 212.0 / 255.0
powderBlue :: Floating a => V3 a
powderBlue = V3 176.0 224.0 230.0 / 255.0
cadetBlue :: Floating a => V3 a
cadetBlue = V3 95.0 158.0 160.0 / 255.0
steelBlue :: Floating a => V3 a
steelBlue = V3 70.0 130.0 180.0 / 255.0
cornFlowerBlue :: Floating a => V3 a
cornFlowerBlue = V3 100.0 149.0 237.0 / 255.0
deepSkyBlue :: Floating a => V3 a
deepSkyBlue = V3 0.0 191.0 255.0 / 255.0
dodgerBlue :: Floating a => V3 a
dodgerBlue = V3 30.0 144.0 255.0 / 255.0
lightBlue :: Floating a => V3 a
lightBlue = V3 173.0 216.0 230.0 / 255.0
skyBlue :: Floating a => V3 a
skyBlue = V3 135.0 206.0 235.0 / 255.0
lightSkyBlue :: Floating a => V3 a
lightSkyBlue = V3 135.0 206.0 250.0 / 255.0
midnightBlue :: Floating a => V3 a
midnightBlue = V3 25.0 25.0 112.0 / 255.0
navy :: Floating a => V3 a
navy = V3 0.0 0.0 128.0 / 255.0
darkBlue :: Floating a => V3 a
darkBlue = V3 0.0 0.0 139.0 / 255.0
mediumBlue :: Floating a => V3 a
mediumBlue = V3 0.0 0.0 205.0 / 255.0
blue :: Floating a => V3 a
blue = V3 0.0 0.0 255.0 / 255.0
royalBlue :: Floating a => V3 a
royalBlue = V3 65.0 105.0 225.0 / 255.0
blueViolet :: Floating a => V3 a
blueViolet = V3 138.0 43.0 226.0 / 255.0
indigo :: Floating a => V3 a
indigo = V3 75.0 0.0 130.0 / 255.0
darkSlateBlue :: Floating a => V3 a
darkSlateBlue = V3 72.0 61.0 139.0 / 255.0
slateBlue :: Floating a => V3 a
slateBlue = V3 106.0 90.0 205.0 / 255.0
mediumSlateBlue :: Floating a => V3 a
mediumSlateBlue = V3 123.0 104.0 238.0 / 255.0
mediumPurple :: Floating a => V3 a
mediumPurple = V3 147.0 112.0 219.0 / 255.0
darkMagenta :: Floating a => V3 a
darkMagenta = V3 139.0 0.0 139.0 / 255.0
darkViolet :: Floating a => V3 a
darkViolet = V3 148.0 0.0 211.0 / 255.0
darkOrchid :: Floating a => V3 a
darkOrchid = V3 153.0 50.0 204.0 / 255.0
mediumOrchid :: Floating a => V3 a
mediumOrchid = V3 186.0 85.0 211.0 / 255.0
purple :: Floating a => V3 a
purple = V3 128.0 0.0 128.0 / 255.0
thistle :: Floating a => V3 a
thistle = V3 216.0 191.0 216.0 / 255.0
plum :: Floating a => V3 a
plum = V3 221.0 160.0 221.0 / 255.0
violet :: Floating a => V3 a
violet = V3 238.0 130.0 238.0 / 255.0
magenta :: Floating a => V3 a
magenta = V3 255.0 0.0 255.0 / 255.0
fuchsia :: Floating a => V3 a
fuchsia = magenta
orchid :: Floating a => V3 a
orchid = V3 218.0 112.0 214.0 / 255.0
mediumVioletRed :: Floating a => V3 a
mediumVioletRed = V3 199.0 21.0 133.0 / 255.0
paleVioletRed :: Floating a => V3 a
paleVioletRed = V3 219.0 112.0 147.0 / 255.0
deepPink :: Floating a => V3 a
deepPink = V3 255.0 20.0 147.0 / 255.0
hotPink :: Floating a => V3 a
hotPink = V3 255.0 105.0 180.0 / 255.0
lightPink :: Floating a => V3 a
lightPink = V3 255.0 182.0 193.0 / 255.0
pink :: Floating a => V3 a
pink = V3 255.0 192.0 203.0 / 255.0
antiqueWhite :: Floating a => V3 a
antiqueWhite = V3 250.0 235.0 215.0 / 255.0
beige :: Floating a => V3 a
beige = V3 245.0 245.0 220.0 / 255.0
bisque :: Floating a => V3 a
bisque = V3 255.0 228.0 196.0 / 255.0
blanchedAlmond :: Floating a => V3 a
blanchedAlmond = V3 255.0 235.0 205.0 / 255.0
wheat :: Floating a => V3 a
wheat = V3 245.0 222.0 179.0 / 255.0
cornSilk :: Floating a => V3 a
cornSilk = V3 255.0 248.0 220.0 / 255.0
lemonChiffon :: Floating a => V3 a
lemonChiffon = V3 255.0 250.0 205.0 / 255.0
lightGoldenRodYellow :: Floating a => V3 a
lightGoldenRodYellow = V3 250.0 250.0 210.0 / 255.0
lightYellow :: Floating a => V3 a
lightYellow = V3 255.0 255.0 224.0 / 255.0
saddleBrown :: Floating a => V3 a
saddleBrown = V3 139.0 69.0 19.0 / 255.0
sienna :: Floating a => V3 a
sienna = V3 160.0 82.0 45.0 / 255.0
chocolate :: Floating a => V3 a
chocolate = V3 210.0 105.0 30.0 / 255.0
peru :: Floating a => V3 a
peru = V3 205.0 133.0 63.0 / 255.0
sandyBrown :: Floating a => V3 a
sandyBrown = V3 244.0 164.0 96.0 / 255.0
burlyWood :: Floating a => V3 a
burlyWood = V3 222.0 184.0 135.0 / 255.0
tan :: Floating a => V3 a
tan = V3 210.0 180.0 140.0 / 255.0
rosyBrown :: Floating a => V3 a
rosyBrown = V3 188.0 143.0 143.0 / 255.0
moccasin :: Floating a => V3 a
moccasin = V3 255.0 228.0 181.0 / 255.0
navajoWhite :: Floating a => V3 a
navajoWhite = V3 255.0 222.0 173.0 / 255.0
peachPuff :: Floating a => V3 a
peachPuff = V3 255.0 218.0 185.0 / 255.0
mistyRose :: Floating a => V3 a
mistyRose = V3 255.0 228.0 225.0 / 255.0
lavenderBlush :: Floating a => V3 a
lavenderBlush = V3 255.0 240.0 245.0 / 255.0
linen :: Floating a => V3 a
linen = V3 250.0 240.0 230.0 / 255.0
oldLace :: Floating a => V3 a
oldLace = V3 253.0 245.0 230.0 / 255.0
papayaWhip :: Floating a => V3 a
papayaWhip = V3 255.0 239.0 213.0 / 255.0
seaShell :: Floating a => V3 a
seaShell = V3 255.0 245.0 238.0 / 255.0
mintCream :: Floating a => V3 a
mintCream = V3 245.0 255.0 250.0 / 255.0
slateGray :: Floating a => V3 a
slateGray = V3 112.0 128.0 144.0 / 255.0
lightSlateGray :: Floating a => V3 a
lightSlateGray = V3 119.0 136.0 153.0 / 255.0
lightSteelBlue :: Floating a => V3 a
lightSteelBlue = V3 176.0 196.0 222.0 / 255.0
lavender :: Floating a => V3 a
lavender = V3 230.0 230.0 250.0 / 255.0
floralWhite :: Floating a => V3 a
floralWhite = V3 255.0 250.0 240.0 / 255.0
aliceBlue :: Floating a => V3 a
aliceBlue = V3 240.0 248.0 255.0 / 255.0
ghostWhite :: Floating a => V3 a
ghostWhite = V3 248.0 248.0 255.0 / 255.0
honeydew :: Floating a => V3 a
honeydew = V3 240.0 255.0 240.0 / 255.0
ivory :: Floating a => V3 a
ivory = V3 255.0 255.0 240.0 / 255.0
azure :: Floating a => V3 a
azure = V3 240.0 255.0 255.0 / 255.0
snow :: Floating a => V3 a
snow = V3 255.0 250.0 250.0 / 255.0
black :: Floating a => V3 a
black = V3 0.0 0.0 0.0 / 255.0
dimGray :: Floating a => V3 a
dimGray = V3 105.0 105.0 105.0 / 255.0
dimGrey :: Floating a => V3 a
dimGrey = dimGray
gray :: Floating a => V3 a
gray = V3 128.0 128.0 128.0 / 255.0
grey :: Floating a => V3 a
grey = gray
darkGray :: Floating a => V3 a
darkGray = V3 169.0 169.0 169.0 / 255.0
darkGrey :: Floating a => V3 a
darkGrey = darkGray
silver :: Floating a => V3 a
silver = V3 192.0 192.0 192.0 / 255.0
lightGray :: Floating a => V3 a
lightGray = V3 211.0 211.0 211.0 / 255.0
lightGrey :: Floating a => V3 a
lightGrey = lightGray
gainsboro :: Floating a => V3 a
gainsboro = V3 220.0 220.0 220.0 / 255.0
whiteSmoke :: Floating a => V3 a
whiteSmoke = V3 245.0 245.0 245.0 / 255.0
white :: Floating a => V3 a
white = V3 255.0 255.0 255.0 / 255.0
