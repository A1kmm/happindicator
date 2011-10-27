import Graphics.UI.AppIndicator
import Graphics.UI.Gtk
import Control.Monad

main = do
  initGUI
  appInd <- appIndicatorNew "appIndicatorDemo" "appointment-soon" AppIndicatorCategoryApplicationStatus
  appIndicatorSetStatus appInd AppIndicatorStatusActive
  set appInd [ appIndicatorAttentionIconName := Just "folder", 
               appIndicatorIconDesc := Just "Demo - non-attention",
               appIndicatorAttentionIconDesc := Just "Demo - attention", 
               appIndicatorLabel := Just "1" ]
  indMenu <- menuNew
  forM_ ["Hello", "World", "I'm a MenuItem"] $ \itemName -> do
    item <- menuItemNewWithLabel itemName
    menuShellAppend indMenu item
    item `on` menuItemActivate $ do
      appIndicatorSetStatus appInd AppIndicatorStatusAttention
    widgetShow item
  widgetShow indMenu
  appIndicatorSetMenu appInd indMenu
  mainGUI
