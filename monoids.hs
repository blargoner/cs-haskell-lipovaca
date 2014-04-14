import qualified Data.Foldable as F
import Data.Monoid

-- a tree
data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Show)

-- a tree is foldable since we can map a function over a tree
-- and use a monoid operation to reduce the resulting values to a single value
instance F.Foldable Tree where
    foldMap f (Leaf x) = f x
    foldMap f (Node l r) = (F.foldMap f l) `mappend` (F.foldMap f r)

-- example
-- sum the numbers in a tree
sumOfTree :: Int
sumOfTree = F.foldr (+) 0 $ Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))

-- example
-- multiply the numbers in a tree
prodOfTree :: Int
prodOfTree = F.foldr (*) 1 $ Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))

-- example
-- search for a number in a tree
-- here we use the Any wrapper type for Bool which is a monoid under disjunction
isInTree :: Bool
isInTree = getAny $ F.foldMap (\x -> Any (x == 3)) $ Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))