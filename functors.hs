import Data.Functor
import Control.Applicative

-- iterate an applicative functor
-- note this is not the same as liftA2 iterate
iterateA :: (Applicative f) => f (a -> a) -> f a -> [f a]
iterateA fs xs = xs : iterateA fs (fs <*> xs)

-- a tree
data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Show)

-- a tree is a functor since we can map a function over a tree
-- the functor laws are satisfied
instance Functor Tree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Node l r) = Node (fmap f l) (fmap f r)

-- a tree is applicative since we can map a tree of functions over a tree
-- the applicative laws are satisfied
instance Applicative Tree where
    pure x = Leaf x
    fs <*> (Leaf x) = fmap ($ x) fs
    fs <*> (Node l r) = Node (fs <*> l) (fs <*> r)

-- example
-- a tree of functions taking the first and second half of a string
-- repeatedly applied to a string yields a tree of substrings
split :: Tree (String -> String)
split = Node
		(Leaf (\s -> let n = length s `div` 2 in take n s))
		(Leaf (\s -> let n = length s `div` 2 in drop n s))

tree :: Tree String
tree = (iterateA split (Leaf "abcdefgh")) !! 3