import Data.List (insertBy)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import System.IO (readFile, writeFile)

data Tree a = Leaf a Int | Branch (Tree a) (Tree a) Int
  deriving (Show)

frequency :: (Ord a) => [a] -> M.Map a Int
frequency xs = M.fromListWith (+) [(x, 1) | x <- xs]

buildQueue :: (Ord a) => M.Map a Int -> [Tree a]
buildQueue frequencies = map (uncurry Leaf) $ M.assocs frequencies

buildHuffmanTree :: (Ord a) => [a] -> Tree a
buildHuffmanTree xs = build (buildQueue $ frequency xs)
  where
    build [t] = t
    build (t1 : t2 : rest) = build (insertBy (comparing rootFrequency) (merge t1 t2) rest)
    merge t1 t2 = Branch t1 t2 (rootFrequency t1 + rootFrequency t2)

buildHuffmanCode :: (Ord a) => Tree a -> M.Map a [Int]
buildHuffmanCode tree = build tree []
  where
    build (Leaf x _) code = M.singleton x code
    build (Branch l r _) code = M.union (build l (code ++ [0])) (build r (code ++ [1]))

rootFrequency :: Tree a -> Int
rootFrequency (Leaf _ freq) = freq
rootFrequency (Branch _ _ freq) = freq

huffman :: (Ord a) => [a] -> M.Map a [Int]
huffman = buildHuffmanCode . buildHuffmanTree

encode :: (Ord a) => M.Map a [Int] -> [a] -> [Int]
encode dict = concatMap (\x -> fromJust $ M.lookup x dict)

decode :: (Ord a) => M.Map a [Int] -> [Int] -> [a]
decode huffDict bits = decode' [] bits 1
  where
    decode' chars [] _ = reverse chars
    decode' chars bits n = case M.lookup (take n bits) revDict of
      Just c -> decode' (c : chars) (drop n bits) 1
      Nothing -> decode' chars bits (n + 1)
    revDict = M.fromList [(b, a) | (a, b) <- M.toList huffDict]

main :: IO ()
main = do
  input <- readFile "input.txt"
  let dict = huffman input

  let encodedData = encode dict input
  writeFile "encoded.txt" (show encodedData)
  encodedInput <- readFile "encoded.txt"
  let loaded = read encodedInput :: [Int]

  let decoded = decode dict loaded
  writeFile "decoded.txt" decoded
  putStrLn "Done!"