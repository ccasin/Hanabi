module Settings.StaticFiles where

import Prelude (IO)
import Yesod.Static
import qualified Yesod.Static as Static
import Settings (staticDir)
import Settings.Development
import ModelTypes

-- | use this to create your static file serving site
staticSite :: IO Static.Static
staticSite = if development then Static.staticDevel staticDir
                            else Static.static      staticDir

-- | This generates easy references to files in the static directory at compile time,
--   giving you compile-time verification that referenced files exist.
--   Warning: any files added to your static directory during run-time can't be
--   accessed this way. You'll have to use their FilePath or URL to access them.
$(staticFiles Settings.staticDir)

smallColorImage :: Color -> StaticRoute
smallColorImage c = 
    case c of
      Red    -> img_red_small_png
      Blue   -> img_blue_small_png
      Green  -> img_green_small_png
      Yellow -> img_yellow_small_png
      Pink   -> img_pink_small_png

cardToRoute :: Card -> StaticRoute
cardToRoute c =
    case c of
      Card Red One      -> img_red1_small_png
      Card Red Two      -> img_red2_small_png      
      Card Red Three    -> img_red3_small_png
      Card Red Four     -> img_red4_small_png
      Card Red Five     -> img_red5_small_png
      Card Blue One     -> img_blue1_small_png
      Card Blue Two     -> img_blue2_small_png      
      Card Blue Three   -> img_blue3_small_png
      Card Blue Four    -> img_blue4_small_png
      Card Blue Five    -> img_blue5_small_png
      Card Green One    -> img_green1_small_png
      Card Green Two    -> img_green2_small_png      
      Card Green Three  -> img_green3_small_png
      Card Green Four   -> img_green4_small_png
      Card Green Five   -> img_green5_small_png
      Card Yellow One   -> img_yellow1_small_png
      Card Yellow Two   -> img_yellow2_small_png      
      Card Yellow Three -> img_yellow3_small_png
      Card Yellow Four  -> img_yellow4_small_png
      Card Yellow Five  -> img_yellow5_small_png
      Card Pink One     -> img_pink1_small_png
      Card Pink Two     -> img_pink2_small_png      
      Card Pink Three   -> img_pink3_small_png
      Card Pink Four    -> img_pink4_small_png
      Card Pink Five    -> img_pink5_small_png

knowledgeToRoute :: Knowledge -> StaticRoute
knowledgeToRoute k =
  case k of
    Knowledge (Is c)      (Is r) -> cardToRoute (Card c r)
    Knowledge (Is Red)    _      -> img_redmystery_small_png
    Knowledge (Is Blue)   _      -> img_bluemystery_small_png
    Knowledge (Is Green)  _      -> img_greenmystery_small_png
    Knowledge (Is Yellow) _      -> img_yellowmystery_small_png
    Knowledge (Is Pink)   _      -> img_pinkmystery_small_png
    Knowledge _       (Is One)   -> img_blank1_small_png
    Knowledge _       (Is Two)   -> img_blank2_small_png
    Knowledge _       (Is Three) -> img_blank3_small_png
    Knowledge _       (Is Four)  -> img_blank4_small_png
    Knowledge _       (Is Five)  -> img_blank5_small_png
    Knowledge _           _      -> img_mystery_small_png       
