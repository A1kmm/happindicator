{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  HAppIndicator
--
--  Author : Andrew Miller (modified from work by Axel Simon)
--
--  Created: 1st October, 2011
--
--  Copyright (C) 2011 Andrew Miller, (C) 2001-2005 Axel Simon
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
-- |
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- This module gathers all publicly available functions from the AppIndicator binding.
--
module Graphics.UI.AppIndicator (
  module Graphics.UI.AppIndicator.AppIndicator,
  module Graphics.UI.AppIndicator.Enums,
) where

import Graphics.UI.AppIndicator.AppIndicator
import Graphics.UI.AppIndicator.Enums
