-- H99
-- http://www.haskell.org/haskellwiki/H-99:_Ninety-Nine_Haskell_Problems

-- last
-- init
-- tail
-- head
-- span
-- group // Data.List
import Data.Bits
import Data.List

-- Q1 リストの最後の要素
myLast :: [a] -> a
myLast [a]  = a   -- myLast (a:[]) = a これでもいい
myLast (a:as) = myLast as
q1 = myLast [1,2,3,4]

-- preludeにはlastがある
myLast' = last


------------------------------------------
-- Q2 リストの最後1つ前
myButLast :: [a] -> a
myButLast (a:b:[]) = a
myButLast (_:as) = myButLast as

myButLast' [_,a] = a
myButLast' (_:as) = myButLast as

q2 = myButLast' [1,2]

-- prelude的には
myButLast'' = last . init


------------------------------------------
-- Q3 リストのn番目の要素 nは1スタート
elementAt :: [a] -> Int -> a
elementAt (a:as) 1 = a
elementAt (a:as) n
 | n < 1     = error "index out of bounds"
 | otherwise = elementAt as (n-1)
elementAt [] _ = error "index out of bounds" -- 空リストでエラーにしておく

q3 = elementAt "haskell" 5


-- Q4 リストの長さ
myLength :: [a] -> Int
myLength as = foldr mycount 0 as
              where
                mycount a b = b+1

myLength' as = foldl (\a -> (\b -> a+1)) 0 as

myLength''' bs = foldl (\a _ -> a+1) 0 bs -- 無名関数の引数はスペース区切り

myLength'' as = myLength''x as 0  -- アキュムレータを使って計算
myLength''x [] n = n
myLength''x (a:as) n = myLength''x as (n+1)

q4 = myLength "haskell"


------------------------------------------
-- Q5 リストを反転
myReverse :: [a] -> [a]
myReverse (a:[]) = [a] 
myReverse (a:as) = (myReverse as) ++ [a] -- これは効率わるいらしい

myReverse' = foldl (flip (:)) [] -- Preludeの定義

q5 = myReverse "haskell hello world"


------------------------------------------
-- Q6 リストで回文判定
isPalindrome :: (Eq a) => [a] -> Bool  -- (==を使うので)
isPalindrome a = a == (reverse a)

q6 = isPalindrome [1,2,1]


------------------------------------------
-- Q7 リストを平らにする
-- haskellのリストは homogeneousなのでデータ型を定義する
data NestedList a = Elem a | List [NestedList a] deriving(Show)

flatten :: NestedList a -> [a]
flatten b = flt [] b
            where
              flt :: [a] -> NestedList a -> [a]
              flt xs (Elem a) = xs ++ [a]
              flt xs (List []) = xs
              flt xs (List (a:as)) = (flt xs a) ++ (flt xs (List as))

q7 = flatten (List [List [Elem 1]])
q7_2 = flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
q7_3 = flatten (List [])


--テスト
data Aaa = Aaa [Int] deriving(Show)
listsum :: Aaa -> Int
listsum (Aaa []) = 0
listsum (Aaa (a:as)) = a + (listsum (Aaa as))
--テスト
data Bbb a = List2 [a] deriving(Show)
listrev :: Bbb a -> [a]
listrev (List2 []) =  []
listrev (List2 (a:as)) = (listrev (List2 as)) ++ [a]

-- List [Elem 1,Elem 2,List [Elem 3, List [Elem 4]]]


------------------------------------------
-- Q8 リストから連続する重複した要素を削除
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:[]) = [x]
compress (x:y:ys)
  | x == y    = compress (y:ys)
  | otherwise = x : (compress (y : ys))

q8 = (compress [1,1,2,3,3,3,3,3,1,0,0,0])
     ++ (compress [])
     ++ (compress [9,9])

------------------------------------------
-- Q9 リストの連続する要素を配列にまとめる
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack xs = pack' [] xs
  where
    pack' as xs = case split xs of
      ([], []) -> as
      (xs, ys) -> pack' (as ++ [xs]) ys

    split :: (Eq a) => [a] -> ([a], [a])
    split [] = ([], [])
    split xs = split' ([],xs)
      where
        split' ([],     (x:xs)) = split' ([x], xs)
        split' ((a:as), (x:xs))
          | a == x    = split' ((x:a:as), xs)
          | otherwise = ((a:as), (x:xs))

-- その二
pack' [] = []
pack' (x:xs) = p [] [x] xs
  where
    p :: (Eq a) => [[a]] -> [a] -> [a] -> [[a]]
    p as (b:bs) [] = as ++ [(b:bs)]
    p as (b:bs) (x:xs)
      | b == x    = p as (x:b:bs) xs
      | otherwise = p (as ++ [(b:bs)]) [x] xs

-- preludeの実装 (Data.Listのgroup関数)
-- span (==2) [2,2,3] = ([2,2],[3])
pack'' [] = []
pack'' (x:xs) =
  let (first,rest) = span (==x) (xs)
  in (x:first) : pack'' rest

-- letで値を変数に束縛すると見易くなる


------------------------------------------
-- Q10 リスト連続した要素をランレングス圧縮

encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode (x:xs) =
  let
    (first, rest) = span (==x) xs
    len = (+1) $ length first
  in  (len, x) : encode rest

-- 解
-- groupを使うとすぐ
encode' xs  = (map (\x -> (length x, head x))) $ group xs


------------------------------------------
-- Q11 Q10のエンコーディングを変更

data Encoding a = Multiple Int a
                | Single a deriving(Show)
                                   
encodeModified :: (Eq a) => [a] -> [Encoding a]
encodeModified xs =
  map enc $ group xs
  where
    enc [x] = Single x
    enc x   = Multiple (length x) (head x)


q11 = encodeModified "aaaabbca,,,aa"


------------------------------------------
-- Q12 Q11でエンコードしたリストをデコード

decodeModified :: [Encoding a] -> [a]
decodeModified xs =
  concat $ map (dec) xs
  where
    dec (Single c) = [c]
    dec (Multiple n c) = replicate n c

-- concatMapでも書ける
decodeModified' = concatMap dec 
  where
    dec (Single c) = [c]
    dec (Multiple n c) = replicate n c

q12 = decodeModified [Single 'a', Multiple 2 'b', Single 'c', Multiple 5 'd']
q12a = decodeModified q11
q12b = decodeModified' q11


-- Q13 ランレングスエンコーディング 直接？
-- サブリストを作るなということみたい
-- preludeを使わず生成しろということか？

encodeDirect :: (Eq a) => [a] -> [Encoding a]
encodeDirect [] = []
encodeDirect (x:xs) =
  reverse $ e [Single x] xs
  where
    e a [] = a
    e (a:as) (x:xs) = 
       if code a == x
         then e ((incr a):as) xs
         else e ((Single x):a:as) xs

    code (Single c)     = c
    code (Multiple n c) = c
    
    incr :: (Encoding a) -> (Encoding a)
    incr (Single c) = Multiple 2 c
    incr (Multiple n c) = Multiple (n+1) c

-- こっちはspan使ってるからダメ？
encodeDirect' :: (Eq a) => [a] -> [Encoding a]
encodeDirect' [] = []
encodeDirect' (x:xs) =
  let
    (first,rest) = span (==x) (xs)
    len = length first + 1
  in
    (e len x) : encodeDirect' rest
    where
      e 1 c = Single c
      e n c = Multiple n c


-- Q14 リストの要素を2回くりかえす

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:(dupli xs)

q14 = dupli [1,2,3]


repli :: [a] ->
