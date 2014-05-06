module Math.Probably.Utils where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

lookupDefault  :: Ord k => a -> k -> Map k a -> a
lookupDefault d k m = fromMaybe d (Map.lookup k m)

