{-# LANGUAGE DeriveFunctor #-}

newtype Fix f = Fix (f (Fix f))

unFix :: Fix f -> f (Fix f)
unFix (Fix x) = x

-- para :: forall f a . (Functor f) => (f (Fix f, a) -> a) -> Fix f -> a
para :: (Functor f) => (f (Fix f, a) -> a) -> Fix f -> a
para ralg = ralg . fmap fanout . unFix
          where  -- fanout :: Fix f -> (Fix f, a)
                 fanout t = (t, para ralg t)
                
data TreeF r = Empty | Branch r r
             deriving (Show, Functor)

type Tree = Fix TreeF

d0  = Fix $ Empty
d1  = Fix $ (Branch d0 d0)
d1a = Fix $ (Branch d1 d0)
d1b = Fix $ (Branch d0 d1)
d2  = Fix $ (Branch (Fix $ Branch d0 d0) d0)
d2a = Fix $ (Branch (Fix $ Branch d0 d0) d0)
d2b = Fix $ (Branch (Fix $ Branch d1 d0) d0)
d2c = Fix $ (Branch (Fix $ Branch d1 d1) d0)

count :: Tree -> Int
count = para alg
      where 
         alg Empty = 0
         alg (Branch (Fix Empty, _)          (Fix Empty, _)         ) = 0
         alg (Branch (Fix (Branch _ _), n) (Fix Empty, _)         ) = 1+n
         alg (Branch (Fix Empty, _)          (Fix (Branch _ _), n)) = n+1
         alg (Branch (Fix (Branch _ _), n) (Fix (Branch _ _), m)) = n+m

main = print $ count d2b