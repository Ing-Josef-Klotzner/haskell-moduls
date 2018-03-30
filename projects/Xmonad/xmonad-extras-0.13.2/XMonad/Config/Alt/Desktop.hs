{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{- |
Module      :  XMonad.Config.Alt.Desktop
Copyright   :  Adam Vogt <vogt.adam@gmail.com>
License     :  BSD3-style (see LICENSE)

Maintainer  :  Adam Vogt <vogt.adam@gmail.com>
Stability   :  unstable
Portability :  unportable

Adapts functionality from some contrib modules

-}
module XMonad.Config.Alt.Desktop (

  -- * "XMonad.Hooks.DynamicLog"
  dzen,
  xmobar,
  statusBar,

  -- * "XMonad.Hooks.EwmhDesktops"
  ewmh,

  -- * "XMonad.Hooks.ManageDocks"
  avoidStrutsOn,
  avoidStruts,


  -- * precedences (apply modifiers in what should be the right order)
  AvoidStrutsPrec, StatusBarPrec,
  EwmhPrec,

  ) where

import qualified XMonad as X
import qualified XMonad.Hooks.EwmhDesktops as E
import qualified XMonad.Hooks.ManageDocks as ManageDocks
import qualified XMonad.Hooks.DynamicLog as DynamicLog
import XMonad.Config.Alt.Internal
import Control.Monad.Trans

$(decNat "avoidStrutsPrec" 1)
$(decNat "statusBarPrec" 2)
$(decNat "ewmhPrec" 6)

-- | See 'E.ewmh'
ewmh c = ins' ewmhPrec hTrue (liftM E.ewmh) c

-- | See 'ManageDocks.avoidStrutsOn'
avoidStrutsOn a c = ins' avoidStrutsPrec hTrue
                  ((m (Proxy :: Proxy Modify) LayoutHook (ManageDocks.avoidStrutsOn a)) =<<)
                  c




-- | See 'ManageDocks.avoidStruts'
avoidStruts c = ins' avoidStrutsPrec hTrue
              (m (Proxy :: Proxy Modify) LayoutHook ManageDocks.avoidStruts =<<)
              c

-- | See 'DynamicLog.statusBar'
-- doesn't set struts
statusBar cmd pp k conf = {- avoidStruts .  -- doesn't typecheck -}
    ins' statusBarPrec hTrue 
                           (\c -> do
                               c' <- c
                               c'' <- liftIO $ DynamicLog.statusBar cmd pp k c'
                               return $ c'' { X.layoutHook = X.layoutHook c' }
                           )
                         $ conf
toggleStrutsKey c = (X.modMask c, X.xK_b)
                                                                   
xmobar conf = statusBar
              "xmobar" 
              DynamicLog.xmobarPP 
              toggleStrutsKey 
              conf
                                                       
dzen conf = statusBar
              ("dzen2" ++ flags)
              DynamicLog.xmobarPP 
              toggleStrutsKey 
              conf
 where
    fg      = "'#a8a3f7'" -- n.b quoting
    bg      = "'#3f3c6d'"
    flags   = "-e 'onstart=lower' -w 400 -ta l -fg " ++ fg ++ " -bg " ++ bg

