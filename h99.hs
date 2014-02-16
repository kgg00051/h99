-- H99
-- http://www.haskell.org/haskellwiki/H-99:_Ninety-Nine_Haskell_Problems

-- Q1 リストの最後の要素
-- preludeにはlastがある
myLast :: [a] -> a
myLast [a]  = a   -- myLast (a:[]) = a これでもいい
myLast (a:as) = myLast as
q1 = myLast [1,2,3,4]



-- Q2 リストの最後1つ前
myButLast :: [a] -> a
myButLast (a:b:[]) = a
myButLast (_:as) = myButLast as

myButLast' [_,a] = a
myButLast' (_:as) = myButLast as

q2 = myButLast' [1,2]


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


-- Q5 リストを反転
myReverse :: [a] -> [a]
myReverse (a:[]) = [a] 
myReverse (a:as) = (myReverse as) ++ [a] -- これは効率わるいらしい

myReverse' = foldl (flip (:)) [] -- Preludeの定義

q5 = myReverse "haskell hello world"


-- Q6 リストで回文判定
isPalindrome :: (Eq a) => [a] -> Bool  -- (==を使うので)
isPalindrome a = a == (reverse a)

q6 = isPalindrome [1,2,1]


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
