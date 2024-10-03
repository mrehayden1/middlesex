module App.Math (
  normalise,

  roundFloat
) where

normalise :: (Ord a, Fractional a) => [a] -> [a]
normalise xs =
  let range = maximum xs - minimum xs
  in fmap ((/ range) . subtract (minimum xs)) xs

roundFloat :: Float -> Float
roundFloat = fromIntegral @Int . round
