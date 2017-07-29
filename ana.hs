{-# LANGUAGE DeriveFunctor #-}

newtype Fix f = Fix (f (Fix f))

unFix :: Fix f -> f (Fix f)
unFix (Fix x) = x

data TreeF r = Empty | Branch r r
             deriving (Show, Functor)

type Tree = Fix TreeF

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

ana :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

plant :: Int -> Tree
plant = ana coalg
      where
          coalg :: Int ->TreeF Int
          coalg 0 = Empty
          coalg n = Branch (n-1) (n-1)


depth :: Tree -> Int
depth = cata alg
    where 
        alg :: TreeF Int -> Int
        alg (Empty) = 0       
        alg (Branch l r) = 1 + max l r

main = print $ depth $ plant 3


