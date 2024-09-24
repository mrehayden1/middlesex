module App.Math (
  normalise
) where

normalise :: (Ord a, Fractional a) => [a] -> [a]
normalise xs =
  let range = maximum xs - minimum xs
  in fmap ((/ range) . subtract (minimum xs)) xs
