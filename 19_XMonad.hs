import XMonad
import XMonad.Actions.Volume
import Data.Map.Lazy (fromList)
import Data.Monoid (mappend)

main = do
  xmonad def { keys =
    \c -> fromList [
      -- when F6 is pressed
      ((0, xK_F6),
        -- reduce volume
        lowerVolume 4 >> return ()),
      -- when F7 is pressed
      ((0, xK_F7),
        -- increase volume
        raiseVolume 4 >> return ())
    ] `mappend` keys defaultConfig c
  }
