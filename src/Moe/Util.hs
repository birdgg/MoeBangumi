module Moe.Util (
  padNumberStart,
) where

padNumberStart :: Int -> Text
padNumberStart n = if n < 10 then "0" <> show n else show n