{-# LANGUAGE DeriveFunctor #-}

newtype Fix f = Fix (f (Fix f))

unFix :: Fix f -> f (Fix f)
unFix (Fix x) = x

data TreeF r = Empty | Branch r r 
             deriving (Show, Functor)

type Tree = Fix TreeF

d0 = Fix $ Empty
d1 = Fix $ (Branch d0 d0)
d2 = Fix $ (Branch (Fix $ Branch d0 d0) d0)

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

depth :: Tree -> Int
depth = cata alg
    where 
        alg (Empty) = 0       
        alg (Branch l r) = 1 + max l r

main = print $ depth d2


