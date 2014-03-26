module Options where

import Data.Aeson.TH

jsonOptions :: Options
jsonOptions = defaultOptions { fieldLabelModifier = tail }
