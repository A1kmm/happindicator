module Graphics.UI.AppIndicator.Enums (
  AppIndicatorCategory,
  AppIndicatorStatus
)
where

{#enum AppIndicatorCategory {underscoreToCase} deriving(Eq,Bounded,Show)#}
{#enum AppIndicatorStatus {underscoreToCase} deriving(Eq,Bounded,Show)#}
