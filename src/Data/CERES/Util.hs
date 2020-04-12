module Data.CERES.Util where


import           Data.Text.Lazy                 ( Text )
import           TextShow


showb1 :: TextShow a => Text -> a -> Builder
showb1 n a = fromLazyText n <> singleton '=' <> showb a
showb2 :: (TextShow a, TextShow b) => Text -> a -> b -> Builder
showb2 n a b =
  fromLazyText n <> singleton '=' <> showb a <> singleton ':' <> showb b
showb3 :: (TextShow a, TextShow b, TextShow c) => Text -> a -> b -> c -> Builder
showb3 n a b c =
  fromLazyText n
    <> singleton '='
    <> showb a
    <> singleton ':'
    <> showb b
    <> singleton ':'
    <> showb c
