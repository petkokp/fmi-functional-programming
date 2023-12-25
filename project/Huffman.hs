import Data.List (insertBy)
import Data.Ord (comparing)
import System.IO (readFile, writeFile)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M

data HuffmanTree a = Leaf a Int | Branch (HuffmanTree a) (HuffmanTree a) Int
                     deriving (Show)

freq :: Ord a => [a] -> M.Map a Int
freq xs = M.fromListWith (+) [(x, 1) | x <- xs]

buildQueue :: Ord a => M.Map a Int -> [HuffmanTree a]
buildQueue freqs = map (uncurry Leaf) $ M.assocs freqs

buildTree :: Ord a => [a] -> HuffmanTree a
buildTree xs = build (buildQueue $ freq xs)
    where
        build [t] = t
        build (t1:t2:rest) = build (insertBy (comparing rootFreq) (merge t1 t2) rest)
        merge t1 t2 = Branch t1 t2 (rootFreq t1 + rootFreq t2)

buildCode :: Ord a => HuffmanTree a -> M.Map a [Int]
buildCode tree = build tree []
    where
        build (Leaf x _) code = M.singleton x code
        build (Branch l r _) code = M.union (build l (code ++ [0])) (build r (code ++ [1]))

rootFreq :: HuffmanTree a -> Int
rootFreq (Leaf _ freq) = freq
rootFreq (Branch _ _ freq) = freq

huffman :: Ord a => [a] -> M.Map a [Int]
huffman = buildCode . buildTree

encode :: Ord a => M.Map a [Int] -> [a] -> [Int]
encode dict xs = concatMap (\x -> fromJust $ M.lookup x dict) xs

decode :: Ord a => M.Map a [Int] -> [Int] -> [a]
decode huffDict bits = decode' [] bits 1
  where
    decode' chars [] _ = reverse chars
    decode' chars bits n = case M.lookup (take n bits) revDict of
      Just c  -> decode' (c:chars) (drop n bits) 1
      Nothing -> decode' chars bits (n+1)
    revDict = M.fromList [(b,a) | (a,b) <- M.toList huffDict]

main :: IO ()
main = do
    content <- readFile "input.txt"
    let dict = huffman content
    let encodedData = encode dict content
    writeFile "encoded.txt" (show encodedData)
    encodedContent <- readFile "encoded.txt"
    let loadedData = read encodedContent :: [Int]
    let decodedData = decode dict loadedData
    writeFile "decoded.txt" decodedData
    putStrLn "Done."