{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MaximizeFocused
  ( MaximizeFocused,
    MaximizeFocusedRestore,
    maximizeFocusedRestore,
    maximizeFocused,
    maximizeFocusedWithPadding,
  )
where

import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.Prelude (partition)
import qualified XMonad.StackSet as S

-- Based on XMonad.Layout.Maximize
-- This version doesn't keep maximized window on focuss loss, so moving away from the
-- maximized window or openning a new one would sink maximized window to it's original
-- position
data MaximizeFocused a = MaximizeFocused Dimension (Maybe Window) deriving (Read, Show)

maximizeFocused :: l Window -> ModifiedLayout MaximizeFocused l Window
maximizeFocused = ModifiedLayout $ MaximizeFocused 25 Nothing

-- | Like 'maximizeFocused', but allows you to specify the amount of padding
-- placed around the maximizeFocusedd window.
maximizeFocusedWithPadding :: Dimension -> l Window -> ModifiedLayout MaximizeFocused l Window
maximizeFocusedWithPadding padding = ModifiedLayout $ MaximizeFocused padding Nothing

newtype MaximizeFocusedRestore = MaximizeFocusedRestore Window deriving (Eq)

instance Message MaximizeFocusedRestore

maximizeFocusedRestore :: Window -> MaximizeFocusedRestore
maximizeFocusedRestore = MaximizeFocusedRestore

instance LayoutModifier MaximizeFocused Window where
  modifierDescription (MaximizeFocused _ (Just _)) = "⮙"
  modifierDescription (MaximizeFocused _ _) = ""

  pureModifier (MaximizeFocused padding (Just target)) rect (Just (S.Stack focused _ _)) wrs =
    if focused == target
      then (maxed ++ rest, Nothing)
      else (wrs, Just (MaximizeFocused padding Nothing))
    where
      (toMax, rest) = partition (\(w, _) -> w == target) wrs
      maxed = map (\(w, _) -> (w, maxRect)) toMax
      maxRect =
        Rectangle
          (rect_x rect + fromIntegral padding)
          (rect_y rect + fromIntegral padding)
          (rect_width rect - padding * 2)
          (rect_height rect - padding * 2)
  pureModifier _ _ _ wrs = (wrs, Nothing)

  pureMess (MaximizeFocused padding mw) m = case fromMessage m of
    Just (MaximizeFocusedRestore w) -> case mw of
      Just w' ->
        if w == w'
          then Just $ MaximizeFocused padding Nothing -- restore window
          else Just $ MaximizeFocused padding $ Just w -- maximizeFocused different window
      Nothing -> Just $ MaximizeFocused padding $ Just w -- maximizeFocused window
    _ -> Nothing
