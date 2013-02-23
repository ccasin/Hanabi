module Settings.StaticFiles where

import Prelude (IO)
import Yesod.Static
import qualified Yesod.Static as Static
import Settings (staticDir)
import Settings.Development
import Model

-- | use this to create your static file serving site
staticSite :: IO Static.Static
staticSite = if development then Static.staticDevel staticDir
                            else Static.static      staticDir

-- | This generates easy references to files in the static directory at compile time,
--   giving you compile-time verification that referenced files exist.
--   Warning: any files added to your static directory during run-time can't be
--   accessed this way. You'll have to use their FilePath or URL to access them.
$(staticFiles Settings.staticDir)

cardToRoute :: Card -> StaticRoute
cardToRoute c =
    case c of
      Card Red One      -> img_red1_png
      Card Red Two      -> img_red2_png      
      Card Red Three    -> img_red3_png
      Card Red Four     -> img_red4_png
      Card Red Five     -> img_red5_png
      Card Blue One     -> img_blue1_png
      Card Blue Two     -> img_blue2_png      
      Card Blue Three   -> img_blue3_png
      Card Blue Four    -> img_blue4_png
      Card Blue Five    -> img_blue5_png
      Card Green One    -> img_green1_png
      Card Green Two    -> img_green2_png      
      Card Green Three  -> img_green3_png
      Card Green Four   -> img_green4_png
      Card Green Five   -> img_green5_png
      Card Yellow One   -> img_yellow1_png
      Card Yellow Two   -> img_yellow2_png      
      Card Yellow Three -> img_yellow3_png
      Card Yellow Four  -> img_yellow4_png
      Card Yellow Five  -> img_yellow5_png
      Card Purple One   -> img_pink1_png
      Card Purple Two   -> img_pink2_png      
      Card Purple Three -> img_pink3_png
      Card Purple Four  -> img_pink4_png
      Card Purple Five  -> img_pink5_png

knowledgeToRoute :: Knowledge -> StaticRoute
knowledgeToRoute k =
  case k of
    Knowledge (Is c)      (Is r) -> cardToRoute (Card c r)
    Knowledge (Is Red)    _      -> img_redmystery_png
    Knowledge (Is Blue)   _      -> img_bluemystery_png
    Knowledge (Is Green)  _      -> img_greenmystery_png
    Knowledge (Is Yellow) _      -> img_yellowmystery_png
    Knowledge (Is Purple) _      -> img_pinkmystery_png
    Knowledge _       (Is One)   -> img_mystery1_png
    Knowledge _       (Is Two)   -> img_mystery2_png
    Knowledge _       (Is Three) -> img_mystery3_png
    Knowledge _       (Is Four)  -> img_mystery4_png
    Knowledge _       (Is Five)  -> img_mystery5_png
    Knowledge _           _      -> img_mystery_png       
