module HashTree where
import Utils
import Hashable32
-- import qualified Control.Monad as CM

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
indent k = showString . f $ replicate k ' '
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

instance (Show a) => Show (MerkleProof a) where
    showsPrec d (MerkleProof e path) =
        showParen (d > 0) $ showString "MerkleProof " . shows e . showChar ' ' . showsMerklePath path

buildProof :: Hashable a => a -> Tree a -> Maybe (MerkleProof a)
buildProof e t = build e . maybeHead $ merklePaths e t
    where
        build :: Hashable a => a -> Maybe MerklePath -> Maybe (MerkleProof a)
        build e Nothing = Nothing
        build e (Just path) = Just (MerkleProof e path)


merklePaths :: Hashable a => a -> Tree a -> [MerklePath]
merklePaths e t = foldr check_and_drop_last [] . all_paths $ t
    where
        h_e = hash e
        all_paths :: Tree a -> [MerklePath]
        all_paths (Leaf h _) = [[Right h]]
        all_paths (Twig _ t) = [Left (treeHash t) : path | path <- all_paths t]
        all_paths (Node _ l r) = [Left (treeHash r) : path | path <- all_paths l] ++ [Right (treeHash l) : path | path <- all_paths r]
        check_and_drop_last :: MerklePath -> [MerklePath] -> [MerklePath]
        check_and_drop_last [] acc = acc
        check_and_drop_last path acc =
            let
                p_init = init path
                p_last = fromEither . last $ path
            in
            if p_last == h_e then p_init:acc else acc


showsMerklePath :: MerklePath -> ShowS
showsMerklePath path = foldr showsStep (showString "") path
    where
        showsStep :: Either Hash Hash -> ShowS -> ShowS
        showsStep (Left h) acc = showString "<" . showsHash h . acc
        showsStep (Right h) acc = showString ">" . showsHash h . acc

showMerklePath :: MerklePath -> String
showMerklePath path = showsMerklePath path ""


verifyProof :: Hashable a => Hash -> MerkleProof a -> Bool
verifyProof h (MerkleProof e path) = foldr check (hash e) path == h
    where
        check :: Either Hash Hash -> Hash -> Hash
        check (Left h) acc = combine acc h
        check (Right h) acc = combine h acc



