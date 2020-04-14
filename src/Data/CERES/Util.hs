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

showbCS1 :: TextShow a => Text -> a -> Builder
showbCS1 n a = fromLazyText n <> singleton ' ' <> showb a
showbCS2 :: (TextShow a, TextShow b) => Text -> a -> b -> Builder
showbCS2 n a b =
  fromLazyText n <> singleton ' ' <> showb a <> singleton ' ' <> showb b
showbCS3
  :: (TextShow a, TextShow b, TextShow c) => Text -> a -> b -> c -> Builder
showbCS3 n a b c =
  fromLazyText n
    <> singleton ' '
    <> showb a
    <> singleton ' '
    <> showb b
    <> singleton ' '
    <> showb c
showbCS4
  :: (TextShow a, TextShow b, TextShow c, TextShow d)
  => Text
  -> a
  -> b
  -> c
  -> d
  -> Builder
showbCS4 n a b c d =
  fromLazyText n
    <> singleton ' '
    <> showb a
    <> singleton ' '
    <> showb b
    <> singleton ' '
    <> showb c
    <> singleton ' '
    <> showb d
showbCS5
  :: (TextShow a, TextShow b, TextShow c, TextShow d, TextShow e)
  => Text
  -> a
  -> b
  -> c
  -> d
  -> e
  -> Builder
showbCS5 n a b c d e =
  fromLazyText n
    <> singleton ' '
    <> showb a
    <> singleton ' '
    <> showb b
    <> singleton ' '
    <> showb c
    <> singleton ' '
    <> showb d
    <> singleton ' '
    <> showb e
showbCS6
  :: (TextShow a, TextShow b, TextShow c, TextShow d, TextShow e, TextShow f)
  => Text
  -> a
  -> b
  -> c
  -> d
  -> e
  -> f
  -> Builder
showbCS6 n a b c d e f =
  fromLazyText n
    <> singleton ' '
    <> showb a
    <> singleton ' '
    <> showb b
    <> singleton ' '
    <> showb c
    <> singleton ' '
    <> showb d
    <> singleton ' '
    <> showb e
    <> singleton ' '
    <> showb f
showbCS7
  :: ( TextShow a
     , TextShow b
     , TextShow c
     , TextShow d
     , TextShow e
     , TextShow f
     , TextShow g
     )
  => Text
  -> a
  -> b
  -> c
  -> d
  -> e
  -> f
  -> g
  -> Builder
showbCS7 n a b c d e f g =
  fromLazyText n
    <> singleton ' '
    <> showb a
    <> singleton ' '
    <> showb b
    <> singleton ' '
    <> showb c
    <> singleton ' '
    <> showb d
    <> singleton ' '
    <> showb e
    <> singleton ' '
    <> showb f
    <> singleton ' '
    <> showb g
showbCS8
  :: ( TextShow a
     , TextShow b
     , TextShow c
     , TextShow d
     , TextShow e
     , TextShow f
     , TextShow g
     , TextShow h
     )
  => Text
  -> a
  -> b
  -> c
  -> d
  -> e
  -> f
  -> g
  -> h
  -> Builder
showbCS8 n a b c d e f g h =
  fromLazyText n
    <> singleton ' '
    <> showb a
    <> singleton ' '
    <> showb b
    <> singleton ' '
    <> showb c
    <> singleton ' '
    <> showb d
    <> singleton ' '
    <> showb e
    <> singleton ' '
    <> showb f
    <> singleton ' '
    <> showb g
    <> singleton ' '
    <> showb h
