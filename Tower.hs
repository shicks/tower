{-# LANGUAGE TupleSections, ViewPatterns #-}

-- Library for winning Spell Tower

import Control.Monad ( forM_ )

import Data.Array ( Array, 
                    array, bounds, elems, indices, ixmap, listArray, 
                    (!), (//) )
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Char8 as S
import Data.Char ( chr, isNumber, isLetter, ord, toUpper )
import Data.Function ( on )
import Data.List ( groupBy, intercalate, nub, nubBy, sort, sortBy )
import Data.Ord ( comparing )
import qualified Data.Trie as T
import Data.Trie ( Trie )

import System.IO ( hFlush, stdout )
import System.IO.Unsafe ( unsafePerformIO )

dictionary :: Trie ()
dictionary = unsafePerformIO $ do
  list <- S.lines `fmap` S.readFile "sowpods.txt" -- TODO(sdh): pass as arg?
  return $ T.fromList $ map ((,()) . S.filter isLetter) list

data Letter = Letter Char Int | Block | Empty
            deriving (Show, Eq, Ord)
type Tower = Array (Int, Int) Letter

showTower :: Tower -> String
showTower = showTower' []

showTower' :: [(Int, Int)] -> Tower -> String
showTower' highlighted tower = unlines $ map showRow [height-1, height-2 .. 0]
  where showRow row = intercalate " " $ map (showCell row) [0 .. width-1]
        showCell row col 
          | (col, row) `elem` highlighted = "**"
          | otherwise = case tower ! (col, row) of
              Letter c 3 -> c:" "
              Letter c n -> c:show n
              Block -> "--"
              Empty -> "  "
        width = (fst $ snd $ bounds tower) + 1
        height = (snd $ snd $ bounds tower) + 1

serializeTower :: Tower -> String
serializeTower tower = intercalate "|" $ map showCol [0 .. width-1]
  where showCol col = concatMap (showCell col) [height-1, height-2 .. 0]
        showCell col row 
          | otherwise = case tower ! (col, row) of
              Letter c 3 -> c:""
              Letter c n -> c:show n
              Block -> "."
              Empty -> ""
        width = (fst $ snd $ bounds tower) + 1
        height = (snd $ snd $ bounds tower) + 1

readTower :: String -> Either String Tower
readTower = readTower' [] [] 
  where readTower' :: [[Letter]] -> [Letter] -> String -> Either String Tower
        readTower' cols col ('.':rest) = readTower' cols (Block:col) rest
        readTower' cols col ('|':rest) = readTower' 
                                         (finishCol (reverse col):cols) [] rest
        readTower' cols col (' ':rest) = readTower' 
                                         (finishCol (reverse col):cols) [] rest
        readTower' cols col ('\n':rest) = readTower' 
                                          (finishCol (reverse col):cols) [] rest
        readTower' cols col (c:n:rest)
          | isLetter c && isNumber n = readTower' cols 
                                       (Letter (toUpper c) (ord n - ord '0'):col) 
                                       rest
          | isLetter c = readTower' cols col (c:'3':n:rest)
          | otherwise = Left $ "Invalid character: " ++ (c:n:rest)
        readTower' cols col (c:[]) = readTower' cols col (c:'3':[])
        readTower' cols col "" = buildTower $ finishCol (reverse col):cols
        finishCol :: [Letter] -> [Letter]
        finishCol col | length col < height = finishCol $ Empty:col
                      | length col > height = finishCol $ tail col
                      | otherwise = reverse col
        buildTower :: [[Letter]] -> Either String Tower
        buildTower cols | length cols < width = buildTower $ finishCol []:cols
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

findWords :: Tower -> [(String, [(Int, Int)], Tower)]
findWords tower = sorted $ 
                  concatMap (findFrom [] S.empty 3 dictionary) $ 
                  indices tower
  where findFrom :: [(Int, Int)] -> ByteString -> Int -> Trie () -> (Int, Int) 
                    -> [(String, [(Int, Int)], Tower)]
        findFrom soFar word size dict ix
          | invalid ix = []
          | otherwise = case tower ! ix of
              Empty -> []
              Block -> []
              Letter l minSize ->
                let word' = S.snoc word l
                    dict' = T.submap word' dict
                    legal = T.lookup word' dict' == Just () &&
                            S.length word' >= max size minSize
                    add = case legal of
                      True -> (option (reverse (ix:soFar)) word':)
                      False -> id
                in if T.null dict'
                   then []
                   else add $ do 
                     n <- neighbors ix
                     case n `elem` (ix:soFar) of
                       True -> []
                       False -> findFrom (ix:soFar) word' 
                                (max size minSize) dict' n
        neighbors (c, r) = do c' <- [c-1, c, c+1]
                              r' <- [r-1, r, r+1]
                              return (c', r')
        immediate (c, r) = [(c-1, r), (c+1, r), (c, r), (c, r-1), (c, r+1)]
        fullRow (_, r) = map (,r) [0 .. width-1]
        word :: [(Int, Int)] -> ([String] -> [String])
        word ixs = case word' 0 "" ixs of
                     (n, w) | length w >= n -> (w:)
                            | otherwise -> id
        word' n w [] = (n, reverse w)
        word' n w (ix:rest) = case tower ! ix of
                                Letter l n' -> word' (max n n') (l:w) rest
                                _ -> error "impossible"
        sorted = sortBy (comparing (negate . length . (\(x,_,_)->x))) . 
                 -- nubBy ((==) `on` fst) . 
                 sortBy (comparing (\(x,_,_)->x))
        width = (fst $ snd $ bounds tower) + 1
        height = (snd $ snd $ bounds tower) + 1
        invalid (c, r) = c < 0 || c >= width || r < 0 || r > height
        option :: [(Int, Int)] -> ByteString -> (String, [(Int, Int)], Tower)
        option ixs word = (S.unpack word, ixs, remove removed tower)
          where removed = nub $ sort $ check ixs $ S.unpack word
                check [] _ = []
                check (i:is) (c:cs) 
                  | c `elem` "JXZQ" = fullRow i ++ check' i is cs
                  | otherwise = check' i is cs
                check' i is cs = filter killed (immediate i) ++ check is cs
                killed i = invalid i || tower ! i == Block || S.length word > 4

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

remove :: [(Int, Int)] -> Tower -> Tower
remove toRemove tower = listArray (bounds tower) $ concatMap remove' $ 
                        groupBy ((==) `on` (fst . fst)) $
                        zip (indices tower) (elems tower)
  where remove' = pad . map snd . filter (not . (`elem` toRemove) . fst)
        pad col 
          | length col < height = col ++ replicate (height - length col) Empty
          | length col > height = error "added something?"
          | otherwise = col
        height = (snd $ snd $ bounds tower) + 1

heights :: Tower -> [Int]
heights tower = map height [0 .. width-1]
  where height col = length $ filter inCol $ zip (indices tower) (elems tower)
          where inCol (_, Empty) = False
                inCol ((c, _), _) = c == col
        width = (fst $ snd $ bounds tower) + 1

showHeights :: Tower -> String
showHeights = concatMap showHeight . heights
  where showHeight n | n < 10 = show n
                     | otherwise = (chr $ ord 'A' + n - 10) : ""

printOptions :: [(String, [(Int, Int)], Tower)] -> IO ()
printOptions results = do
  forM_ results $ \(word, _, tower') -> do
    putStrLn $ show word ++ " " ++ showHeights tower'
    forM_ (findWords tower') $ \(word', _, tower'') ->
      if not $ word' `elem` map (\(x,_,_) -> x) results
      then putStrLn $ show word ++ " " ++ show word' ++ " " ++ showHeights tower''
      else return ()

printScores :: Int -> [(String, [(Int, Int)], Tower)] -> IO ()
printScores count results = let options1 = map (scoreResult []) results
                                options2 = concatMap scoreNextResult results
                            in putStrLn $ unlines $ map snd $ 
                               take count $ nub $ sort $ options1 ++ options2
  where scoreResult :: [String] -> (String, a, Tower) -> (Int, String)
        scoreResult ws (w, _, t) = (negate $ score (w:ws) $ heights t, 
                                    unwords $ reverse $ showHeights t:w:ws)
        scoreNextResult :: (String, a, Tower) -> [(Int, String)]
        scoreNextResult (w, _, t) = map (scoreResult [w]) $ filter isNew $ 
                                    findWords t
        isNew :: (String, a, b) -> Bool
        isNew (w', _, _) = not $ any (\(w, _, _) -> w' == w) $ results

score :: [String] -> [Int] -> Int
score words heights = lengths - importance * variance - over9penalty
  where lengths = average (map length words) + maximum (map length words)
        minHeight = minimum heights
        variance = average $ map abs $ diff $ minHeight : heights ++ [minHeight]
        diff a = map (uncurry (-)) $ zip a $ tail a
        average a = sum a `div` length a
        over9penalty = 2 * (min 0 $ maximum heights - 9)
        importance = min 1 $ maximum heights - 7

showHelp :: IO ()
showHelp = putStrLn $ unlines [
  "Commands:",
  "  p          Print the tower.",
  "  s          Print top 40 results.",
  "  1          Print all results.",
  "  2          Print all results two levels deep.",
  "  a <line>   Adds a line to the bottom.",
  "  r <tower>  Redefines the tower.",
  "  w <word>   Removes the word.",
  "  q          Quits."]

repl :: Tower -> IO ()
repl tower = do
  let results = findWords tower
  putStr "> "
  hFlush stdout
  line <- getLine
  case line of
    ('q':_) -> putStrLn $ serializeTower tower
    ('?':_) -> showHelp >> repl tower
    ('p':_) -> putStrLn (showTower tower) >> repl tower
    ('s':_) -> printScores 40 results >> repl tower
    ('1':_) -> do forM_ results $ \(w,_,t) -> putStrLn $ show w ++ " " ++ 
                                              showHeights t
                  repl tower
    ('2':_) -> printOptions results >> repl tower
    ('a':' ':row) -> repl $ addRow row tower
    ('r':' ':spec) -> case readTower spec of
      Left err -> putStrLn ("Error: " ++ err) >> repl tower
      Right tower' -> repl tower'
    ('w':' ':word) ->
      case filter (\(x,_,_) -> x == map toUpper word) results of
        [] -> putStrLn "Not a valid word." >> repl tower
        [(_, ixs, tower')] -> do putStrLn $ showTower' ixs tower
                                 repl tower'
        ts -> do
          which <- let get :: IO Int
                       get = do
                         forM_ (zip [1..] $ map (\(_,ixs,_)->ixs) ts) $ 
                           \(i, ixs) -> do
                             putStrLn $ show i ++ "."
                             putStrLn $ showTower' ixs tower
                         putStr "disambiguate> "
                         hFlush stdout
                         answer <- getLine
                         case answer of
                           ('q':_) -> return 0
                           _ -> case reads answer of
                             [(d, "")] -> return d
                             _ -> get
                   in get
          if which == 0 
            then repl tower
            else repl $ (\(_,_,t)->t) $ ts !! (which - 1)
    _ -> putStrLn "Unknown command" >> repl tower

main :: IO ()
main = repl $ (\(Right x)->x) $ readTower ""

  -- print $ map (\(a,b) -> (a,concatMap show $ heights b)) $ findWords tower
  -- putStrLn ""
  -- let tower2 = addRow "gj4wxv.u5s" tower
  -- putStrLn $ showTower tower2
  -- print $ elems tower2
