{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MarkEwmhFullscreen (markEwmhFullscreen) where

import Control.Monad (unless)
import Graphics.X11
import XMonad
import XMonad.Layout.LayoutModifier
import qualified XMonad.StackSet as W

data MarkEwmhFullscreen a = MarkEwmhFullscreen
  deriving (Show, Read)

instance LayoutModifier MarkEwmhFullscreen Window where
  -- Set the fullscreen state on all windows in the layout
  modifyLayout MarkEwmhFullscreen ws@(W.Workspace _ _ s) rect = do
    case s of
      Just stack -> mapM_ setFullscreenState (W.integrate stack)
      Nothing -> pure ()
    runLayout ws rect

  -- Remove fullscreen property from all windows in the layout
  unhook MarkEwmhFullscreen = do
    ws <- (gets windowset :: X WindowSet)
    let windows = (W.stack . W.workspace . W.current $ ws) :: Maybe (W.Stack Window)
    case windows of
      Nothing -> pure ()
      Just xs -> do
        mapM_ removeFullscreenState (W.integrate xs)

setFullscreenState :: Window -> X ()
setFullscreenState w = do
  dpy <- asks display

  wmState <- io $ internAtom dpy "_NET_WM_STATE" False
  fullscreen <- io $ internAtom dpy "_NET_WM_STATE_FULLSCREEN" False

  currentState <- io $ getWindowProperty32 dpy wmState w
  case currentState of
    Just atoms -> do
      unless (fromIntegral fullscreen `elem` atoms) $ do
        io $ changeProperty32 dpy w wmState aTOM propModeAppend [fromIntegral fullscreen]
    Nothing -> do
      io $ changeProperty32 dpy w wmState aTOM propModeReplace [fromIntegral fullscreen]

removeFullscreenState :: Window -> X ()
removeFullscreenState w = do
  dpy <- asks display

  wmState <- io $ internAtom dpy "_NET_WM_STATE" False
  fullscreen <- io $ internAtom dpy "_NET_WM_STATE_FULLSCREEN" False

  currentState <- io $ getWindowProperty32 dpy wmState w
  case currentState of
    Just atoms -> do
      let filteredAtoms = filter (/= fromIntegral fullscreen) atoms
      if null filteredAtoms
        then io $ deleteProperty dpy w wmState
        else io $ changeProperty32 dpy w wmState aTOM propModeReplace filteredAtoms
    Nothing -> pure () -- Nothing to remove

markEwmhFullscreen :: l Window -> ModifiedLayout MarkEwmhFullscreen l Window
markEwmhFullscreen = ModifiedLayout MarkEwmhFullscreen
