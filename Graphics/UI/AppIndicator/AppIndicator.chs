{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  libappindicator widget AppIndicator
--
--  Author : Andrew Miller
--
--  Created: 1st October 2011
--
--  Copyright (C) 2011 Andrew Miller
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
-- Create and manipulate menus and icons in the app indications area (as used in
-- Unity). This code will fall back on using a status icon.
--
module Graphics.UI.AppIndicator.AppIndicator (

-- * Detail
--
-- | The applications indication area is used for menus that provide some
-- kind of status about the system; there are several predefined menus (for
-- example, relating to sound, mail), and additional ones can be added. On
-- platforms not supporting this functionality, the library will fallback
-- to using a status icon (see Graphics.UI.Gtk.Display.StatusIcon in the
-- gtk package).

-- * Class Hierarchy
--
-- |
-- @
-- |  'GObject'
-- |   +----AppIndicator
-- @

-- * Types
  AppIndicator,
  AppIndicatorClass,
  castToAppIndicator, gTypeAppIndicator,
  toAppIndicator,

-- * Constructors
  appIndicatorNew,
  appIndicatorNewWithPath,

-- * Methods
  appIndicatorBuildMenuFromDesktop,
  appIndicatorGetMenu,
  appIndicatorSetMenu,
  appIndicatorGetStatus,
  appIndicatorSetStatus,
  appIndicatorGetCategory,
  
-- * Attributes
  appIndicatorAttentionIconDesc,
  appIndicatorAttentionIconName,
  appIndicatorCategory,
  appIndicatorConnected,
  appIndicatorIconDesc,
  appIndicatorIconName,
  appIndicatorIconThemePath,
  appIndicatorId,
  appIndicatorLabel,
  appIndicatorLabelGuide,
  appIndicatorOrderingIndex,
  appIndicatorStatus,

-- * Signals
  appIndicatorNewIcon,
  appIndicatorNewAttentionIcon,
  appIndicatorNewStatus,
  appIndicatorNewLabel,
  appIndicatorConnectionChanged,
  appIndicatorNewIconThemePath,
  appIndicatorScrollEvent,
  ) where

import Control.Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
import System.Glib.GObject		(wrapNewGObject,makeNewGObject)
{#import Graphics.UI.AppIndicator.Types#}
{#import Graphics.UI.AppIndicator.Enums#}
{#import Graphics.UI.AppIndicator.Signals#}
{#import Graphics.UI.Gtk#}
{#import Graphics.UI.GtkInternals#}

{# context lib="appindicator" prefix="app_indicator" #}

--------------------
-- Constructors

-- | Creates an empty AppIndicator object, setting the properties: "id" with id,
-- | "category" with category and "icon-name" with icon_name.
appIndicatorNew :: String -> String -> AppIndicatorCategory -> IO AppIndicator
appIndicatorNew id iconName  cat =
  wrapNewGObject mkAppIndicator $ withUTFString id $ \idPtr ->
    withUTFString iconName $ \iconNamePtr ->
      {# call app_indicator_new #}
        idPtr iconNamePtr ((fromIntegral . fromEnum) cat)

-- | Creates a new AppIndicator setting the properties: "id" with id, "category"
-- | with category, "icon-name" with icon_name and "icon-theme-path" with
-- | icon_theme_path. 
appIndicatorNewWithPath :: String -> String -> AppIndicatorCategory -> String -> IO AppIndicator
appIndicatorNewWithPath id iconName category iconThemePath =
  wrapNewGObject mkAppIndicator $ withUTFString id $ \idPtr ->
    withUTFString iconName $ \iconNamePtr -> withUTFString iconThemePath $
      \iconThemePathPtr ->
        {# call app_indicator_new_with_path #}
          idPtr iconNamePtr ((fromIntegral . fromEnum)category) iconThemePathPtr

-- | This function allows for building the Application Indicator menu from a static
-- | desktop file. 
appIndicatorBuildMenuFromDesktop :: AppIndicatorClass self => self -> String -> String -> IO ()
appIndicatorBuildMenuFromDesktop self desktopFile desktopProfile =
  withUTFString desktopFile $ \desktopFilePtr ->
    withUTFString desktopProfile $ \desktopProfilePtr ->
     {# call app_indicator_build_menu_from_desktop #}
       (toAppIndicator self) desktopFilePtr desktopProfilePtr

-- | This function retrieves the Application Indicator menu.
appIndicatorGetMenu :: AppIndicatorClass self => self -> IO Menu
appIndicatorGetMenu self =
  makeNewGObject mkMenu $ {# call app_indicator_get_menu #} (toAppIndicator self)

-- | This function sets the Application Indicator menu.
appIndicatorSetMenu :: (AppIndicatorClass self, MenuClass menu) => self -> menu -> IO ()
appIndicatorSetMenu self menu =
  {# call app_indicator_set_menu #}
    (toAppIndicator self) (toMenu menu)

-- | This function retrieves the current status of the Application Indicator.
appIndicatorGetStatus :: AppIndicatorClass self => self -> IO AppIndicatorStatus
appIndicatorGetStatus self =
  liftM (toEnum . fromIntegral) $ {# call app_indicator_get_status #} (toAppIndicator self)

-- | This function set the status of the Application Indicator.
appIndicatorSetStatus :: AppIndicatorClass self => self -> AppIndicatorStatus -> IO ()
appIndicatorSetStatus self stat =
  {# call app_indicator_set_status #}
    (toAppIndicator self) ((fromIntegral . fromEnum) stat)

-- | This function retrieves the category of the Application Indicator.
appIndicatorGetCategory :: AppIndicatorClass self => self -> IO AppIndicatorCategory
appIndicatorGetCategory self =
  liftM (toEnum . fromIntegral) $ {# call app_indicator_get_category #} (toAppIndicator self)

-- * Attributes

-- | If the indicator sets it's status to APP_INDICATOR_STATUS_ATTENTION then this textual description of the icon shown. 
appIndicatorAttentionIconDesc :: AppIndicatorClass self => Attr self (Maybe String)
appIndicatorAttentionIconDesc = newAttrFromMaybeStringProperty "attention-icon-desc"

-- | If the indicator sets it's status to APP_INDICATOR_STATUS_ATTENTION then this icon is shown. 
appIndicatorAttentionIconName :: AppIndicatorClass self => Attr self (Maybe String)
appIndicatorAttentionIconName = newAttrFromMaybeStringProperty "attention-icon-name"

-- | The type of indicator that this represents. Please don't use 'Other'. Defaults to 'ApplicationStatus'.
appIndicatorCategory :: AppIndicatorClass self => Attr self (Maybe String)
appIndicatorCategory = newAttrFromMaybeStringProperty "category"

-- | Pretty simple, TRUE if we have a reasonable expectation of being displayed through this object. You should hide your TrayIcon if so. 
appIndicatorConnected :: AppIndicatorClass self => ReadAttr self Bool
appIndicatorConnected = readAttrFromBoolProperty "connected"

-- | The description of the regular icon that is shown for the indicator. 
appIndicatorIconDesc :: AppIndicatorClass self => Attr self (Maybe String)
appIndicatorIconDesc = newAttrFromMaybeStringProperty "icon-desc"

-- | The name of the regular icon that is shown for the indicator. 
appIndicatorIconName :: AppIndicatorClass self => Attr self (Maybe String)
appIndicatorIconName = newAttrFromMaybeStringProperty "icon-name"

-- | An additional place to look for icon names that may be installed by the application.
appIndicatorIconThemePath :: AppIndicatorClass self => Attr self (Maybe String)
appIndicatorIconThemePath = newAttrFromMaybeStringProperty "icon-theme-path"

-- | The ID for this indicator, which should be unique, but used consistently by this program and its indicator.
appIndicatorId :: AppIndicatorClass self => Attr self (Maybe String)
appIndicatorId = newAttrFromMaybeStringProperty "id"

-- | A label that can be shown next to the string in the application indicator. The label will not be shown unless there is an icon as well. The label is useful for numerical and other frequently updated information. In general, it shouldn't be shown unless a user requests it as it can take up a significant amount of space on the user's panel. This may not be shown in all visualizations.
appIndicatorLabel :: AppIndicatorClass self => Attr self (Maybe String)
appIndicatorLabel = newAttrFromMaybeStringProperty "label"

-- | An optional string to provide guidance to the panel on how big the "label" string could get. If this is set correctly then the panel should never 'jiggle' as the string adjusts through out the range of options. For instance, if you were providing a percentage like "54% thrust" in "label" you'd want to set this string to "100% thrust" to ensure space when Scotty can get you enough power.
appIndicatorLabelGuide :: AppIndicatorClass self => Attr self (Maybe String)
appIndicatorLabelGuide = newAttrFromMaybeStringProperty "label-guide"

-- | The ordering index is an odd parameter, and if you think you don't need it you're probably right. In general, the application indicator try to place the applications in a recreatable place taking into account which category they're in to try and group them. But, there are some cases where you'd want to ensure indicators are next to each other. To do that you can override the generated ordering index and replace it with a new one. Again, you probably don't want to be doing this, but in case you do, this is the way.
appIndicatorOrderingIndex :: AppIndicatorClass self => Attr self Int
appIndicatorOrderingIndex = newAttrFromUIntProperty "ordering-index"

-- | Whether the indicator is shown or requests attention. Defaults to 'Passive'.
appIndicatorStatus :: AppIndicatorClass self => Attr self (Maybe String)
appIndicatorStatus = newAttrFromMaybeStringProperty "status"

-- * Signals

-- | Emitted when "icon-name" is changed 
appIndicatorNewIcon :: AppIndicatorClass self => Signal self (IO ())
appIndicatorNewIcon = Signal (connect_NONE__NONE "new-icon")

-- | Emitted when "attention-icon-name" is changed 
appIndicatorNewAttentionIcon :: AppIndicatorClass self => Signal self (IO ())
appIndicatorNewAttentionIcon = Signal (connect_NONE__NONE "new-attention-icon")

-- | Emitted when "status" is changed
appIndicatorNewStatus :: AppIndicatorClass self => Signal self (String -> IO ())
appIndicatorNewStatus = Signal (connect_STRING__NONE "new-status")

-- | Emitted when either "label" or "label-guide" are changed. 
appIndicatorNewLabel :: AppIndicatorClass self => Signal self (String -> String -> IO ())
appIndicatorNewLabel = Signal (connect_STRING_STRING__NONE "new-label")

-- | Emitted when we connect to a watcher, or when it drops away. 
appIndicatorConnectionChanged :: AppIndicatorClass self => Signal self (Bool -> IO ())
appIndicatorConnectionChanged = Signal (connect_BOOL__NONE "connection-changed")

-- | Emitted when there is a new icon set for the object. 
appIndicatorNewIconThemePath :: AppIndicatorClass self => Signal self (String -> IO ())
appIndicatorNewIconThemePath = Signal (connect_STRING__NONE "new-icon-theme-path")

-- | Emitted when the AppIndicator receives a scroll event.
appIndicatorScrollEvent :: AppIndicatorClass self => Signal self (Int -> Int -> IO ())
appIndicatorScrollEvent = Signal (connect_INT_INT__NONE "scroll-event")
