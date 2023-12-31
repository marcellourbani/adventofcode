-- couldn't find a way to import this as a module in stack scripts, will just copy and paste
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImportQualifiedPost #-}

module AocLibrary (GameMap, mapTile) where

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

data GameMap c = GameMap {gmW :: Int, gmH :: Int, gmMap :: M.Map (Int, Int) c} deriving (Eq)

instance Functor GameMap where
  fmap f (GameMap w h m) = GameMap w h $ M.map f m

instance Show (GameMap Char) where
  show gm@(GameMap w h m) = unlines [[mapTile '.' gm (x, y) | x <- [0 .. w - 1]] | y <- [0 .. h - 1]]

instance {-# OVERLAPPABLE #-} Show c => Show (GameMap c) where
  show gm@(GameMap w h m) = show $ gm {gmMap = head . show <$> m}

mapTile :: c -> GameMap c -> (Int, Int) -> c
mapTile def gm p = fromMaybe def $ M.lookup p $ gmMap gm
