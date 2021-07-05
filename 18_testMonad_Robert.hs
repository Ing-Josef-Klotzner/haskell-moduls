{-# LANGUAGE PartialTypeSignatures #-}

list :: [(Int,String,Char)]
list = do
  a <- [1..10]
  c <- ['a' .. 'z']
  let b = show a ++ show c
  pure (a,b,c)

list' :: List (Int, Char)
list' = do
  a <- Cons 1 (Cons 2 (Cons 3 Nil))
  b <- Cons 'a' (Cons 'b' (Cons 'c' Nil))
  pure (a,b)

list'' :: List (Int, Char)
list'' = Cons 1 (Cons 2 (Cons 3 Nil))
         >>=  \ a -> Cons 'a' (Cons 'b' (Cons 'c' Nil))
                >>= \ b -> Cons (a,b) Nil

-- ([1,2,3,4,5,6,7,8,9,10],,]


data List a = Nil | Cons a (List a) deriving Show

l1 :: List String
l1 = Cons "a" Nil

l2 :: List Int
l2 = Cons 1 (Cons 7 Nil)

l3 :: List Int
l3 = Cons 2 (Cons 3 Nil)

append :: List a -> List a -> List a
append Nil bs = bs
append as Nil = as
append (Cons a as) bs = Cons a (append as bs)

concat' :: List (List a) -> List a
concat' Nil = Nil
concat' (Cons Nil as) = concat' as
concat' (Cons a as) = append a (concat' as)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  Cons a as <*> bs = (a <$> bs) `append` (as <*> bs)

instance Monad List where
  Nil >>= _   = Nil
  as  >>= f   = concat' $ fmap f as
