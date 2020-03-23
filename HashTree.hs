module HashTree where
import Utils
import Hashable32

data Tree a = Leaf Hash a | Twig Hash (Tree a) | Node Hash (Tree a) (Tree a)

leaf :: Hashable a => a -> Tree a
leaf e = Leaf (hash e) e

twig :: Hashable a => Tree a -> Tree a
twig t = Twig h_root t
    where
        h_t = treeHash t
        h_root = combine h_t h_t

node :: Hashable a => Tree a -> Tree a -> Tree a
node l r = Node h_root l r
    where
        h_root = combine (treeHash l) (treeHash r)

buildTree :: Hashable a => [a] -> Tree a
buildTree [] = error "buildTree: empty list"
buildTree l = build . map leaf $ l
    where
        build :: Hashable a => [Tree a] -> Tree a
        build [] = error "build: empty list"
        build [t] = t
        build list = build . pair_trees $ list
        pair_trees :: Hashable a => [Tree a] -> [Tree a]
        pair_trees [] = []
        pair_trees [t] = [twig t]
        pair_trees (t1:t2:rest) = node t1 t2 : pair_trees rest

treeHash :: Tree a -> Hash
treeHash (Leaf h _)   = h
treeHash (Twig h _)   = h
treeHash (Node h _ _) = h

indent :: Int -> ShowS
indent k = showString . f . take k . repeat $ ' '
    where
        f = if k == 0 then id else showChar '\n'

drawsTree :: Show a => Tree a -> Int -> ShowS
drawsTree (Leaf h e) k = indent k . showsHash h . showChar ' ' . shows e
drawsTree (Twig h t) k = indent k . showsHash h . showString " +" . drawsTree t (k + 2)
drawsTree (Node h l r) k = indent k . showsHash h . showString " -" . drawsTree l (k + 2) . drawsTree r (k + 2)

drawTree :: Show a => Tree a -> String
drawTree t = drawsTree t 0 "\n"


type MerklePath = [Either Hash Hash]

data MerkleProof a = MerkleProof a MerklePath

buildProof :: Hashable a => a -> Tree a -> Maybe (MerkleProof a)
buildProof = undefined

merklePaths :: Hashable a => a -> Tree a -> [MerklePath]
merklePaths = undefined

verifyProof :: Hashable a => Hash -> MerkleProof a -> Bool
verifyProof = undefined



