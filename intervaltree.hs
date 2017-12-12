module IntervalTree
    ( ITree(Leaf, Node),
      listTree, foldLeaves, size, lvl, treeList,
      makeTree, set, get, put
    ) where

import Data.List
import Control.Exception


pairMap :: (a -> a -> b) -> [a] -> [b]
pairMap _ [] = []
pairMap f (x:y:xs) = f x y : pairMap f xs

const2 :: (a -> b -> c -> a)
const2 x _ _ = x

--------------------------------------------------------------------------------

data ITree a b = Leaf a | Node b (ITree a b) (ITree a b)
     deriving (Eq, Show, Read)

listTree :: (b -> b -> b) -> (a -> a -> b) -> [a] -> ITree a b
listTree g h xs = buildNodes $ map (\x -> Leaf x) xs
  where
    buildNodes [t] = t
    buildNodes ts  = buildNodes $ pairMap mapper ts
    mapper l@(Leaf lv)     r@(Leaf rv)     = Node (h lv rv) l r
    mapper l@(Node lv _ _) r@(Node rv _ _) = Node (g lv rv) l r

foldLeaves :: (a->c) -> (c->c->c) -> ITree a b -> c
foldLeaves f _ (Leaf x)       = f x
foldLeaves f g (Node _ lt rt) = g (foldLeaves f g lt) (foldLeaves f g rt)

size :: ITree a b -> Int
size (Leaf _) = 1
size (Node _ l _) = 2 * size l

lvl :: ITree a b -> ([[b]], [a])
lvl t = lvl' [t] []
  where
    -- lvl' :: [ITree a b] -> (([[b]], [a]), [ITree a b])
    lvl' [] [] = ([],[])
    lvl' [] qs = let (xss, ls) = lvl' (reverse qs) [] in ([]:xss, ls)
    lvl' ts@(Leaf _:_) _ = ([], map (\(Leaf x)->x) ts)
    lvl' (Node x l r : ts) qs =
      let ((xs:xss), ls) = lvl' ts (r:l:qs) in (((x:xs):xss), ls)

--------------------------------------------------------------------------------

treeList = foldLeaves (\x->[x]) (++)

makeTree = listTree (const2 0) (const2 0)

get :: Num a => ITree a a -> Int -> a
get t i = get' (size t) t i

get' 1  (Leaf x) 1 = x
get' ss (Node x l r) i
  | i <= s    = x + get' s l i
  | otherwise = x + get' s r (i - s)
  where s = ss `div` 2

set :: Num a => ITree a a -> (Int, a) -> ITree a a
set t (i, v) = put t (i, i, v)

put :: Num a => ITree a a -> (Int, Int, a) -> ITree a a
put t (l, r, v) = put' (size t) t l r v

put' 1  (Leaf x) 1 1 v = Leaf (x+v)
put' ss (Node x lt rt) l r v
  | 1 == l && r == ss  = Node (x+v) lt                rt
  | r <= s             = Node x     (put' s lt l r v) rt
  |           s <  l   = Node x     lt                (put' s rt (l-s) (r-s) v)
  | otherwise          = Node x     (put' s lt l s v) (put' s rt 1     (r-s) v)
  where s = ss `div` 2

-------------------------------------------------------------------------------

_main = let
    t = makeTree $ replicate 16 1
    (ns, ls) = lvl $ (t `put` (2,15,8)) `put` (4,7,1)
    sz = 4 * size t
    mapshow _ [] = []
    mapshow sz (n:ns) = let
        hsz = sz `div` 2
        space = let s = replicate (hsz-2) '-' in s++"   "++s
        hspace = take (hsz-1) space
        line = hspace ++ intercalate space n ++ hspace
      in line : mapshow hsz ns
  in putStr $ unlines $ mapshow sz $ (map (map show) ns) ++ [ map show ls ]

main = putStr test_put

tester :: (Show a, Eq a) => [a] -> [a] -> String
tester xs ys =
  assert ((length xs) == (length ys)) (let
      ress = zipWith (==) xs ys
      res = foldl (&&) True ress
    in if res
          then "Tests passed\n"
          else unlines [ show xs, show ys, show ress ])

test_put = let
    trees =
      [
        (makeTree $ replicate 16 1 ) `put` (2,15,8)
      ]
    expect =
      [
        Node 0 (
          Node 0 (
              Node 0 (Node 0 (Leaf 1) (Leaf 9)) (Node 8 (Leaf 1) (Leaf 1))
            ) (
              Node 8 (Node 0 (Leaf 1) (Leaf 1)) (Node 0 (Leaf 1) (Leaf 1))
            )
          ) (
          Node 0 (
              Node 8 (Node 0 (Leaf 1) (Leaf 1)) (Node 0 (Leaf 1) (Leaf 1))
            ) (
              Node 0 (Node 8 (Leaf 1) (Leaf 1)) (Node 0 (Leaf 9) (Leaf 1)
            )
          )
        )
      ]
  in tester trees expect
