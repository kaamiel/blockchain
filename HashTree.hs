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
        pair_trees (t1:t2:ts) = node t1 t2 : pair_trees ts

treeHash :: Tree a -> Hash
treeHash (Leaf h _) = h
treeHash (Twig h _) = h
treeHash (Node h _ _) = h

{- | drawsTree
>>> putStr $ drawTree $ buildTree "fubar"
0x2e1cc0e4 -
  0xfbfe18ac -
    0x6600a107 -
      0x00000066 'f'
      0x00000075 'u'
    0x62009aa7 -
      0x00000062 'b'
      0x00000061 'a'
  0xd11bea20 +
    0x7200b3e8 +
      0x00000072 'r'

>>> print $ drawTree $ buildTree "a"
"0x00000061 'a'\n"
-}

indent :: Int -> ShowS
indent k
    | k <= 0    = id
    | otherwise = showString . showChar '\n' $ replicate k ' '

drawsTree :: Show a => Tree a -> Int -> ShowS
drawsTree (Leaf h e) k = indent k . showsHash h . showChar ' ' . shows e
drawsTree (Twig h t) k = indent k . showsHash h . showString " +" . drawsTree t (k + 2)
drawsTree (Node h l r) k = indent k . showsHash h . showString " -" . drawsTree l (k + 2) . drawsTree r (k + 2)

drawTree :: Show a => Tree a -> String
drawTree t = drawsTree t 0 "\n"


{- | Merkle Paths & proofs
>>> mapM_ print $ map showMerklePath  $ merklePaths 'i' $ buildTree "bitcoin"
"<0x5214666a<0x7400b6ff>0x00000062"
">0x69f4387c<0x6e00ad98>0x0000006f"

>>> merklePaths 'i' $ buildTree "bitcoin"
[[Left 1377068650,Left 1946203903,Right 98],[Right 1777612924,Left 1845538200,Right 111]]

>>> buildProof 'i' $ buildTree "bitcoin"
Just (MerkleProof 'i' <0x5214666a<0x7400b6ff>0x00000062)

>>> buildProof 'e' $ buildTree "bitcoin"
Nothing

>>> let t = buildTree "bitcoin"
>>> let proof = buildProof 'i' t
>>> verifyProof (treeHash t) <$> proof
Just True
>>> verifyProof 0xbada55bb <$> proof
Just False

>>> buildProof 'a' $ buildTree "a"
Just (MerkleProof 'a' )
-}

type MerklePath = [Either Hash Hash]
data MerkleProof a = MerkleProof a MerklePath

instance (Show a) => Show (MerkleProof a) where
    showsPrec d (MerkleProof e path) =
        showParen (d > 0) $ showString "MerkleProof " . showsPrec 11 e . showChar ' ' . showsMerklePath path

buildProof :: Hashable a => a -> Tree a -> Maybe (MerkleProof a)
buildProof e t = build e . maybeHead $ merklePaths e t
    where
        build :: Hashable a => a -> Maybe MerklePath -> Maybe (MerkleProof a)
        build _ Nothing = Nothing
        build e' (Just path) = Just (MerkleProof e' path)

merklePaths :: Hashable a => a -> Tree a -> [MerklePath]
merklePaths e t = foldr check_and_drop_last [] . all_paths $ t
    where
        h_e = hash e
        all_paths :: Tree a -> [MerklePath]
        all_paths (Leaf h _) = [[Right h]]
        all_paths (Twig _ t') = [Left (treeHash t') : path | path <- all_paths t']
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
showsMerklePath path = foldr showsStep id path
    where
        showsStep :: Either Hash Hash -> ShowS -> ShowS
        showsStep (Left h) acc = showString "<" . showsHash h . acc
        showsStep (Right h) acc = showString ">" . showsHash h . acc

showMerklePath :: MerklePath -> String
showMerklePath path = showsMerklePath path ""

verifyProof :: Hashable a => Hash -> MerkleProof a -> Bool
verifyProof h (MerkleProof e path) = foldr step (hash e) path == h
    where
        step :: Either Hash Hash -> Hash -> Hash
        step (Left h') acc = combine acc h'
        step (Right h') acc = combine h' acc
