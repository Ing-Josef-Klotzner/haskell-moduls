--15_Exercise_Optional_Monoid.hs
module Optional_Monoid where
import Data.Monoid

data Optional a = Nada | Only a deriving (Eq, Show)
instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    mappend Nada a = a
    mappend a Nada = a
    Only a `mappend` Only b = Only (a `mappend` b)

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String
madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj = e <> "! he said " <> adv <> " as he jumped into his car " <>
    noun <> " and drove off with his " <> adj <> " wife."

-- Rewrite it using mconcat

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj = mconcat [e, "! he said ", adv, " as he jumped into his car ",
    noun, " and drove off with his ", adj, " wife."]

e = "Wonderful"
adv = "critical"
noun = "Honda"
adj = "nice"

-- *Optional_Monoid> madlibbinBetter' e adv noun adj
--"Wonderful! he said critical as he jumped into his car Honda and drove off with his nice wife."

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)
instance Monoid (First' a) where
    mempty = First' Nada
    First' Nada `mappend` First' Nada = First' Nada
    First' Nada `mappend` First' (Only a) = First' (Only a)
    First' (Only a) `mappend` _ = First' (Only a)

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool
type FstId = First' String -> Bool

--Prelude> First' (Only 1) `mappend` First' Nada
--First' {getFirst' = Only 1}
--Prelude> First' Nada `mappend` First' Nada
--First' {getFirst' = Nada}
--Prelude> First' Nada `mappend` First' (Only 2)
--First' {getFirst' = Only 2}
--Prelude> First' (Only 1) `mappend` First' (Only 2)
--First' {getFirst' = Only 1}



