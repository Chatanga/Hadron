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
    , chartReuse
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

maroon :: Floating a => V3 a
maroon = V3 128 0 0 / 255
darkRed :: Floating a => V3 a
darkRed = V3 139 0 0 / 255
brown :: Floating a => V3 a
brown = V3 165 42 42 / 255
firebrick :: Floating a => V3 a
firebrick = V3 178 34 34 / 255
crimson :: Floating a => V3 a
crimson = V3 220 20 60 / 255
red :: Floating a => V3 a
red = V3 255 0 0 / 255
tomato :: Floating a => V3 a
tomato = V3 255 99 71 / 255
coral :: Floating a => V3 a
coral = V3 255 127 80 / 255
indianRed :: Floating a => V3 a
indianRed = V3 205 92 92 / 255
lightCoral :: Floating a => V3 a
lightCoral = V3 240 128 128 / 255
darkSalmon :: Floating a => V3 a
darkSalmon = V3 233 150 122 / 255
salmon :: Floating a => V3 a
salmon = V3 250 128 114 / 255
lightSalmon :: Floating a => V3 a
lightSalmon = V3 255 160 122 / 255
orangeRed :: Floating a => V3 a
orangeRed = V3 255 69 0 / 255
darkOrange :: Floating a => V3 a
darkOrange = V3 255 140 0 / 255
orange :: Floating a => V3 a
orange = V3 255 165 0 / 255
gold :: Floating a => V3 a
gold = V3 255 215 0 / 255
darkGoldenRod :: Floating a => V3 a
darkGoldenRod = V3 184 134 11 / 255
goldenRod :: Floating a => V3 a
goldenRod = V3 218 165 32 / 255
paleGoldenRod :: Floating a => V3 a
paleGoldenRod = V3 238 232 170 / 255
darkKhaki :: Floating a => V3 a
darkKhaki = V3 189 183 107 / 255
khaki :: Floating a => V3 a
khaki = V3 240 230 140 / 255
olive :: Floating a => V3 a
olive = V3 128 128 0 / 255
yellow :: Floating a => V3 a
yellow = V3 255 255 0 / 255
yellowGreen :: Floating a => V3 a
yellowGreen = V3 154 205 50 / 255
darkOliveGreen :: Floating a => V3 a
darkOliveGreen = V3 85 107 47 / 255
oliveDrab :: Floating a => V3 a
oliveDrab = V3 107 142 35 / 255
lawnGreen :: Floating a => V3 a
lawnGreen = V3 124 252 0 / 255
chartReuse :: Floating a => V3 a
chartReuse = V3 127 255 0 / 255
greenYellow :: Floating a => V3 a
greenYellow = V3 173 255 47 / 255
darkGreen :: Floating a => V3 a
darkGreen = V3 0 100 0 / 255
green :: Floating a => V3 a
green = V3 0 128 0 / 255
forestGreen :: Floating a => V3 a
forestGreen = V3 34 139 34 / 255
lime :: Floating a => V3 a
lime = V3 0 255 0 / 255
limeGreen :: Floating a => V3 a
limeGreen = V3 50 205 50 / 255
lightGreen :: Floating a => V3 a
lightGreen = V3 144 238 144 / 255
paleGreen :: Floating a => V3 a
paleGreen = V3 152 251 152 / 255
darkSeaGreen :: Floating a => V3 a
darkSeaGreen = V3 143 188 143 / 255
mediumSpringGreen :: Floating a => V3 a
mediumSpringGreen = V3 0 250 154 / 255
springGreen :: Floating a => V3 a
springGreen = V3 0 255 127 / 255
seaGreen :: Floating a => V3 a
seaGreen = V3 46 139 87 / 255
mediumAquaMarine :: Floating a => V3 a
mediumAquaMarine = V3 102 205 170 / 255
mediumSeaGreen :: Floating a => V3 a
mediumSeaGreen = V3 60 179 113 / 255
lightSeaGreen :: Floating a => V3 a
lightSeaGreen = V3 32 178 170 / 255
darkSlateGray :: Floating a => V3 a
darkSlateGray = V3 47 79 79 / 255
teal :: Floating a => V3 a
teal = V3 0 128 128 / 255
darkCyan :: Floating a => V3 a
darkCyan = V3 0 139 139 / 255
aqua :: Floating a => V3 a
aqua = V3 0 255 255 / 255
cyan :: Floating a => V3 a
cyan = V3 0 255 255 / 255
lightCyan :: Floating a => V3 a
lightCyan = V3 224 255 255 / 255
darkTurquoise :: Floating a => V3 a
darkTurquoise = V3 0 206 209 / 255
turquoise :: Floating a => V3 a
turquoise = V3 64 224 208 / 255
mediumTurquoise :: Floating a => V3 a
mediumTurquoise = V3 72 209 204 / 255
paleTurquoise :: Floating a => V3 a
paleTurquoise = V3 175 238 238 / 255
aquaMarine :: Floating a => V3 a
aquaMarine = V3 127 255 212 / 255
powderBlue :: Floating a => V3 a
powderBlue = V3 176 224 230 / 255
cadetBlue :: Floating a => V3 a
cadetBlue = V3 95 158 160 / 255
steelBlue :: Floating a => V3 a
steelBlue = V3 70 130 180 / 255
cornFlowerBlue :: Floating a => V3 a
cornFlowerBlue = V3 100 149 237 / 255
deepSkyBlue :: Floating a => V3 a
deepSkyBlue = V3 0 191 255 / 255
dodgerBlue :: Floating a => V3 a
dodgerBlue = V3 30 144 255 / 255
lightBlue :: Floating a => V3 a
lightBlue = V3 173 216 230 / 255
skyBlue :: Floating a => V3 a
skyBlue = V3 135 206 235 / 255
lightSkyBlue :: Floating a => V3 a
lightSkyBlue = V3 135 206 250 / 255
midnightBlue :: Floating a => V3 a
midnightBlue = V3 25 25 112 / 255
navy :: Floating a => V3 a
navy = V3 0 0 128 / 255
darkBlue :: Floating a => V3 a
darkBlue = V3 0 0 139 / 255
mediumBlue :: Floating a => V3 a
mediumBlue = V3 0 0 205 / 255
blue :: Floating a => V3 a
blue = V3 0 0 255 / 255
royalBlue :: Floating a => V3 a
royalBlue = V3 65 105 225 / 255
blueViolet :: Floating a => V3 a
blueViolet = V3 138 43 226 / 255
indigo :: Floating a => V3 a
indigo = V3 75 0 130 / 255
darkSlateBlue :: Floating a => V3 a
darkSlateBlue = V3 72 61 139 / 255
slateBlue :: Floating a => V3 a
slateBlue = V3 106 90 205 / 255
mediumSlateBlue :: Floating a => V3 a
mediumSlateBlue = V3 123 104 238 / 255
mediumPurple :: Floating a => V3 a
mediumPurple = V3 147 112 219 / 255
darkMagenta :: Floating a => V3 a
darkMagenta = V3 139 0 139 / 255
darkViolet :: Floating a => V3 a
darkViolet = V3 148 0 211 / 255
darkOrchid :: Floating a => V3 a
darkOrchid = V3 153 50 204 / 255
mediumOrchid :: Floating a => V3 a
mediumOrchid = V3 186 85 211 / 255
purple :: Floating a => V3 a
purple = V3 128 0 128 / 255
thistle :: Floating a => V3 a
thistle = V3 216 191 216 / 255
plum :: Floating a => V3 a
plum = V3 221 160 221 / 255
violet :: Floating a => V3 a
violet = V3 238 130 238 / 255
magenta :: Floating a => V3 a
magenta = V3 255 0 255 / 255
fuchsia :: Floating a => V3 a
fuchsia = magenta
orchid :: Floating a => V3 a
orchid = V3 218 112 214 / 255
mediumVioletRed :: Floating a => V3 a
mediumVioletRed = V3 199 21 133 / 255
paleVioletRed :: Floating a => V3 a
paleVioletRed = V3 219 112 147 / 255
deepPink :: Floating a => V3 a
deepPink = V3 255 20 147 / 255
hotPink :: Floating a => V3 a
hotPink = V3 255 105 180 / 255
lightPink :: Floating a => V3 a
lightPink = V3 255 182 193 / 255
pink :: Floating a => V3 a
pink = V3 255 192 203 / 255
antiqueWhite :: Floating a => V3 a
antiqueWhite = V3 250 235 215 / 255
beige :: Floating a => V3 a
beige = V3 245 245 220 / 255
bisque :: Floating a => V3 a
bisque = V3 255 228 196 / 255
blanchedAlmond :: Floating a => V3 a
blanchedAlmond = V3 255 235 205 / 255
wheat :: Floating a => V3 a
wheat = V3 245 222 179 / 255
cornSilk :: Floating a => V3 a
cornSilk = V3 255 248 220 / 255
lemonChiffon :: Floating a => V3 a
lemonChiffon = V3 255 250 205 / 255
lightGoldenRodYellow :: Floating a => V3 a
lightGoldenRodYellow = V3 250 250 210 / 255
lightYellow :: Floating a => V3 a
lightYellow = V3 255 255 224 / 255
saddleBrown :: Floating a => V3 a
saddleBrown = V3 139 69 19 / 255
sienna :: Floating a => V3 a
sienna = V3 160 82 45 / 255
chocolate :: Floating a => V3 a
chocolate = V3 210 105 30 / 255
peru :: Floating a => V3 a
peru = V3 205 133 63 / 255
sandyBrown :: Floating a => V3 a
sandyBrown = V3 244 164 96 / 255
burlyWood :: Floating a => V3 a
burlyWood = V3 222 184 135 / 255
tan :: Floating a => V3 a
tan = V3 210 180 140 / 255
rosyBrown :: Floating a => V3 a
rosyBrown = V3 188 143 143 / 255
moccasin :: Floating a => V3 a
moccasin = V3 255 228 181 / 255
navajoWhite :: Floating a => V3 a
navajoWhite = V3 255 222 173 / 255
peachPuff :: Floating a => V3 a
peachPuff = V3 255 218 185 / 255
mistyRose :: Floating a => V3 a
mistyRose = V3 255 228 225 / 255
lavenderBlush :: Floating a => V3 a
lavenderBlush = V3 255 240 245 / 255
linen :: Floating a => V3 a
linen = V3 250 240 230 / 255
oldLace :: Floating a => V3 a
oldLace = V3 253 245 230 / 255
papayaWhip :: Floating a => V3 a
papayaWhip = V3 255 239 213 / 255
seaShell :: Floating a => V3 a
seaShell = V3 255 245 238 / 255
mintCream :: Floating a => V3 a
mintCream = V3 245 255 250 / 255
slateGray :: Floating a => V3 a
slateGray = V3 112 128 144 / 255
lightSlateGray :: Floating a => V3 a
lightSlateGray = V3 119 136 153 / 255
lightSteelBlue :: Floating a => V3 a
lightSteelBlue = V3 176 196 222 / 255
lavender :: Floating a => V3 a
lavender = V3 230 230 250 / 255
floralWhite :: Floating a => V3 a
floralWhite = V3 255 250 240 / 255
aliceBlue :: Floating a => V3 a
aliceBlue = V3 240 248 255 / 255
ghostWhite :: Floating a => V3 a
ghostWhite = V3 248 248 255 / 255
honeydew :: Floating a => V3 a
honeydew = V3 240 255 240 / 255
ivory :: Floating a => V3 a
ivory = V3 255 255 240 / 255
azure :: Floating a => V3 a
azure = V3 240 255 255 / 255
snow :: Floating a => V3 a
snow = V3 255 250 250 / 255
black :: Floating a => V3 a
black = V3 0 0 0 / 255
dimGray :: Floating a => V3 a
dimGray = V3 105 105 105 / 255
dimGrey :: Floating a => V3 a
dimGrey = dimGray
gray :: Floating a => V3 a
gray = V3 128 128 128 / 255
grey :: Floating a => V3 a
grey = gray
darkGray :: Floating a => V3 a
darkGray = V3 169 169 169 / 255
darkGrey :: Floating a => V3 a
darkGrey = darkGray
silver :: Floating a => V3 a
silver = V3 192 192 192 / 255
lightGray :: Floating a => V3 a
lightGray = V3 211 211 211 / 255
lightGrey :: Floating a => V3 a
lightGrey = lightGray
gainsboro :: Floating a => V3 a
gainsboro = V3 220 220 220 / 255
whiteSmoke :: Floating a => V3 a
whiteSmoke = V3 245 245 245 / 255
white :: Floating a => V3 a
white = V3 255 255 255 / 255
