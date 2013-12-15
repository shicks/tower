{-# LANGUAGE TupleSections, ViewPatterns #-}

-- Library for winning Spell Tower

import Data.Array ( Array, array, bounds, indices, ixmap, (!), (//) )
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Char8 as S
import Data.Char ( isNumber, isLetter, ord, toUpper )
import Data.List ( intercalate, nub, sort, sortBy )
import Data.Ord ( comparing )
import qualified Data.Trie as T
import Data.Trie ( Trie )

import System.IO.Unsafe ( unsafePerformIO )

dictionary :: Trie ()
dictionary = unsafePerformIO $ do
  list <- S.lines `fmap` S.readFile "sowpods.txt" -- TODO(sdh): pass as arg?
  return $ T.fromList $ map ((,()) . S.filter isLetter) list

data Letter = Letter Char Int | Block | Empty
            deriving (Show, Eq, Ord)
type Tower = Array (Int, Int) Letter

showTower :: Tower -> String
showTower tower = unlines $ map showRow [height-1, height-2 .. 0]
  where showRow row = intercalate " " $ map (showCell row) [0 .. width-1]
        showCell row col = case tower ! (col, row) of
          Letter c 3 -> c:" "
          Letter c n -> c:show n
          Block -> "--"
          Empty -> "  "
        width = (fst $ snd $ bounds tower) + 1
        height = (snd $ snd $ bounds tower) + 1

readTower :: String -> Either String Tower
readTower = readTower' [] [] 
  where readTower' :: [[Letter]] -> [Letter] -> String -> Either String Tower
        readTower' cols col ('.':rest) = readTower' cols (Block:col) rest
        readTower' cols col ('|':rest) = readTower' (finishCol col:cols) [] rest
        readTower' cols col ('\n':rest) = readTower' (finishCol col:cols) [] rest
        readTower' cols col (c:n:rest)
          | isLetter c && isNumber n = readTower' cols 
                                       (Letter (toUpper c) (ord n - ord '0'):col) 
                                       rest
          | isLetter c = readTower' cols col (c:'3':n:rest)
          | otherwise = Left $ "Invalid character: " ++ (c:n:rest)
        readTower' cols col (c:[]) = readTower' cols col (c:'3':[])
        readTower' cols col "" = buildTower $ finishCol col:cols
        finishCol :: [Letter] -> [Letter]
        finishCol col | length col < height = finishCol $ Empty:col
                      | length col > height = finishCol $ tail col
                      | otherwise = reverse col
        buildTower :: [[Letter]] -> Either String Tower
        buildTower cols | length cols < width = buildTower $ []:cols
                        | length cols > width = buildTower $ tail cols 
                        | otherwise = buildTower' $ reverse cols
        buildTower' cols = Right $
                           array ((0, 0), (width - 1, height - 1)) $
                           concatMap (uncurry index) $ zip [0..] cols
        index :: Int -> [Letter] -> [((Int, Int), Letter)]
        index c col = map (\(r, letter) -> ((c, r), letter)) $ zip [0..] col
        width, height :: Int
        width = 8
        height = 12

findWords :: Tower -> [String]
findWords tower = sorted $ 
                  concatMap (findFrom [] S.empty 3 dictionary) $ 
                  indices tower
  where findFrom :: [(Int, Int)] -> ByteString -> Int -> Trie () -> (Int, Int) 
                    -> [String]
        findFrom soFar word size dict ix@(c, r)
          | c < 0 || c >= width || r < 0 || r > height = []
          | otherwise = case tower ! ix of
                          Empty -> []
                          Block -> []
                          Letter l minSize ->
                            let word' = S.snoc word l
                                dict' = T.submap word' dict
                                rest = if T.null dict'
                                       then []
                                       else do 
                                         n <- neighbors ix
                                         case n `elem` (ix:soFar) of
                                           True -> []
                                           False -> findFrom (ix:soFar) word' 
                                                    (max size minSize) dict' n
                            in case T.lookup word dict of
                              Just () 
                                | S.length word >= size -> S.unpack word:rest
                                | otherwise -> rest
                              _ -> rest
        neighbors (c, r) = do c' <- [c-1, c, c+1]
                              r' <- [r-1, r, r+1]
                              return (c', r')
        word :: [(Int, Int)] -> ([String] -> [String])
        word ixs = case word' 0 "" ixs of
                     (n, w) | length w >= n -> (w:)
                            | otherwise -> id
        word' n w [] = (n, reverse w)
        word' n w (ix:rest) = case tower ! ix of
                                Letter l n' -> word' (max n n') (l:w) rest
                                _ -> error "impossible"
        sorted = sortBy (comparing (negate . length)) . nub . sort
        width = (fst $ snd $ bounds tower) + 1
        height = (snd $ snd $ bounds tower) + 1

-- Silently discards the top row
addRow :: String -> Tower -> Tower
addRow row tower = ixmap (bounds tower) (\(c, r) -> (c, (r-1) `mod` height)) $
                   tower // parseRow 0 [] row
  where height = (snd $ snd $ bounds tower) + 1
        width = (fst $ snd $ bounds tower) + 1
        parseRow i ls "" = ls
        parseRow i ls ('.':rest) = parseRow (i+1) (((i, height-1), Block):ls) rest
        parseRow i ls (c:d:rest) 
          | isLetter c && isNumber d = let n = ord d - ord '0'
                                       in parseRow (i+1) 
                                          (((i, height-1), 
                                            Letter (toUpper c) n):ls) rest
          | isLetter c = parseRow i ls (c:'3':d:rest)
        parseRow i ls (c:[]) = parseRow i ls (c:'3':[])

main :: IO ()
main = do
  -- First we need to read the tower
  tower <- (readTower `fmap` getContents) >>= either fail return
  putStrLn $ showTower tower
  putStrLn ""
  let tower2 = addRow "gj4wxv.u5s" tower
  putStrLn $ showTower tower2
  print $ findWords tower2
