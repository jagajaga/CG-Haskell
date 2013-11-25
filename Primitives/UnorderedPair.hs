module Primitives.UnorderedPair(UnorderedPair,
                                makeUnorderedPair,
                                other,
                                isElem) where

import           Data.Hashable

data UnorderedPair a = UnorderedPair !a !a deriving (Show, Eq)

makeUnorderedPair :: (Ord a) => a -> a -> UnorderedPair a
makeUnorderedPair a b
    | a <= b = UnorderedPair a b
    | otherwise = UnorderedPair b a

instance (Hashable a, Ord a) => Hashable (UnorderedPair a) where
  hashWithSalt s (UnorderedPair a b) = hashWithSalt s (a, b)

other :: (Eq a) => a -> UnorderedPair a -> Maybe a
other x (UnorderedPair a b)
    | x == a  = Just b
    | x == b = Just a
    | otherwise =  Nothing

isElem :: (Eq a) => a -> UnorderedPair a -> Bool
isElem x p = case other x p of
  Just _ -> True ; _ -> False
