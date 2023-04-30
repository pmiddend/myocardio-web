module Myocardio.Util (textShow, maximum, utcTimeDayDiff) where

import Data.Foldable (Foldable (foldMap))
import Data.Function ((.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Maybe (Maybe (Just))
import Data.Ord (Ord)
import Data.Semigroup (Max (Max, getMax))
import Data.Text
  ( Text,
    pack,
  )
import Data.Time (UTCTime)
import Data.Time.Clock (diffUTCTime)
import Data.Tuple (fst)
import Text.Show
  ( Show,
    show,
  )
import Prelude (Integral (div), RealFrac (properFraction))

textShow :: Show a => a -> Text
textShow = pack . show

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = (getMax <$>) . foldMap (Just . Max)

utcTimeDayDiff :: UTCTime -> UTCTime -> Int
utcTimeDayDiff now t = fst (properFraction (now `diffUTCTime` t)) `div` 86400
