{-# LANGUAGE InstanceSigs #-}
import Data.Bifunctor
import Data.Functor.Const
import Data.Functor.Identity

main :: IO ()
main = return ()

-- 1. Show that the data type:
--   data Pair a b = Pair a b
-- is a bifunctor. For additional credit implement all three methods of
-- Bifunctor and use equational reasoning to show that these definitions are
-- compatible with the default implementations whenever they can be applied.
data Pair a b = Pair a b
instance Bifunctor Pair where
    bimap :: (a -> c) -> (b -> d) -> Pair a b -> Pair c d
    bimap f g (Pair a b) = Pair (f a) (g b)
    first :: (a -> b) -> Pair a c -> Pair b c
    first f (Pair a c) = Pair (f a) c
    second :: (b -> c) -> Pair a b -> Pair a c
    second f (Pair a b) = Pair a (f b)

-- 2. Show the isomorphism between the standard definition of Maybe and this
-- desugaring:
--   type Maybe' a = Either (Const () a) (Identity a)
-- Hint: Define two mappings between the two implementations. For additional
-- credit, show that they are the inverse of each other using equational
-- reasoning.
type Maybe' a = Either (Const () a) (Identity a)

maybeToMaybe' :: Maybe a -> Maybe' a
maybeToMaybe' Nothing = Left (Const ())
maybeToMaybe' (Just a) = Right (Identity a)

maybe'ToMaybe :: Maybe' a -> Maybe a
maybe'ToMaybe (Left _) = Nothing
maybe'ToMaybe (Right (Identity a)) = Just a

-- 3. Let’s try another data structure. I call it a PreList because it’s a
-- precursor to a List. It replaces recursion with a type parameter b.
--   data PreList a b = Nil | Cons a b
-- You could recover our earlier definition of a List by recursively applying
-- PreList to itself (we’ll see how it’s done when we talk about fixed points).
-- Show that PreList is an instance of Bifunctor.
--
-- We already showed that Cons is a bifunctor in task 1. Nil is functor,
-- coproduct (`|`) is a bifunctor, so PreList is a bifunctor too.

-- 4. Show that the following data types define bifunctors in a and b:
-- data K2 c a b = K2 c
-- data Fst a b = Fst a
-- data Snd a b = Snd b
-- For additional credit, check your solutions against Conor
-- McBride’s paper Clowns to the Left of me, Jokers to the Right.
data K2 c a b = K2 c
data Fst a b = Fst a
data Snd a b = Snd b
instance Bifunctor (K2 c) where
    bimap f g (K2 c) = K2 c
instance Bifunctor Fst where
    bimap f g (Fst a) = Fst (f a)
instance Bifunctor Snd where
    bimap f g (Snd b) = Snd (g b)
