module Ch5
  (
  , test
  )
  where

import Data.List (List(..), (:))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude (type (~>), Unit, discard, max, negate, otherwise, show, (+), (-), (/=), (<), (<<<), (==), (>), (>=))


flip :: ∀ a b c. (a -> b -> c) -> b -> (a -> c)
flip f x = \y -> f y x
const :: ∀ a b. a -> b -> a
const x _ = x

apply :: ∀ a b. (a -> b) -> a -> b
apply f x = f x

infixr 0 apply as $

applyFlipped :: ∀ a b. a -> (a -> b) -> b
applyFlipped = flip apply

infixl 1 applyFlipped as #

singleton :: ∀ a. a -> List a
singleton x = x : Nil

cons :: ∀ a. a -> List a -> List a
cons x xs = Cons x xs

null :: ∀ a. List a -> Boolean
null Nil = true
null _ = false

-- Goの appendに相当する
snoc :: ∀ a. List a -> a -> List a
snoc Nil x = singleton x
snoc (y : ys) x = y : snoc ys x

length :: ∀ a. List a -> Int
length Nil = 0
length (_ : xs) = 1 + length xs

head :: ∀ a. List a -> Maybe a
head Nil = Nothing
head (x : _) = Just x

tail :: ∀ a. List a -> Maybe (List a)
tail Nil = Nothing
tail (_ : xs) = Just xs

last :: ∀ a. List a -> Maybe a
last Nil = Nothing
last (x : Nil) = Just x
last (_ : xs) = last xs

init :: ∀ a. List a -> Maybe (List a)
init Nil = Nothing
init l = Just $ go l where
  go :: List a -> List a
  go Nil = Nil
  go (_ : Nil) = Nil
  go (x : xs) = x : go xs

uncons :: ∀ a. List a -> Maybe {head :: a, tail :: List a}
uncons Nil = Nothing
uncons (x : xs) = Just {head: x, tail: xs}

index :: ∀ a. List a -> Int -> Maybe a
index Nil _ = Nothing
index (_ : _) i | i < 0 = Nothing
index (x : _) 0 = Just x
index (_ : xs) i = index xs (i-1)
infixl 8 index as !!

findIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findIndex pred l = go 0 l where
  go :: Int -> List a -> Maybe Int
  go _ Nil = Nothing
  go acc (x : xs) = if pred x then Just acc else go (acc + 1) xs

findLastIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex pred l = go Nothing 0 l where
  go :: Maybe Int -> Int -> List a -> Maybe Int
  go fi _ Nil = fi
  go fi i (x : xs) = if pred x then go (Just i) (i+1) xs else go fi (i + 1) xs

reverse :: List ~> List
reverse l = go Nil l where
  go acc Nil = acc
  go acc (x : xs) = go (x : acc) xs

concat :: ∀ a. List (List a) -> List a
concat Nil = Nil
concat (Nil : xss) = concat xss
concat ((x : xs) : xss) = x : concat (xs : xss)

filter :: ∀ a. (a -> Boolean) -> List a -> List a
-- filter pred l = reverse $ go (Nil :: List a) l  where
--   go acc Nil = acc
--   go acc (x : xs) = if pred x then go (x : acc) xs else go acc xs
filter pred = reverse <<< go Nil where
  go :: List a -> List a -> List a
  go nl Nil = nl
  go nl (x : xs) = if pred x then go (x : nl) xs else go nl xs

catMaybes :: ∀ a. List (Maybe a) -> List a
catMaybes l = reverse $ go (Nil :: List a) l where
  go :: List a -> List (Maybe a) -> List a
  go acc Nil = acc
  go acc (mx : mxs) = case mx of
    Just x -> go (x : acc) mxs
    Nothing -> go acc mxs
catMaybes' :: ∀ a. List (Maybe a) -> List a
catMaybes' Nil = Nil
catMaybes' (mx : mxs) = case mx of
  Just x -> x : catMaybes' mxs
  Nothing -> catMaybes' mxs

range :: Int -> Int -> List Int
range start end =
  let step = if start < end then 1 else (-1) in
  if start == end then singleton start
  else start : range (start + step) end

-- compiler optimization 1 
-- with keeping out of the recursive call by
-- step calculation
range' :: Int -> Int -> List Int
range' start end = go start where
  go cur | cur == end = singleton cur -- cur means current
         | otherwise = cur : go (cur + step)
  step = if start < end then 1 else (-1)

-- compiler optimization 2
-- with tail recursion

range'' :: Int -> Int -> List Int
range'' start end = go (Nil :: List Int) end start where
  go :: List Int -> Int -> Int -> List Int
  -- We name "cur" as "current from end",
  -- "ter" as "terminal pointing start".
  -- this "go" recursion "going back" to start from end.
  go acc cur ter | cur == ter = cur : acc
                 | otherwise = go (cur : acc) (cur + step) ter
  step = if start < end then (-1) else 1 

take :: ∀ a. Int -> List a -> List a
-- max 0 nで 負の整数が与えられたときに、無限stackに陥らないようにする.
take n = reverse <<< go (max 0 n) (Nil :: List a) where
  go :: Int -> List a -> List a -> List a
  go _ acc Nil = acc
  go 0 acc _ = acc
  go i acc (x : xs) = go (i - 1) (x : acc) xs

take' :: ∀ a. Int -> List a -> List a
take' n = go (max 0 n) where
  go :: Int -> List a -> List a
  go _ Nil = Nil
  go 0 _ = Nil
  go i (x : xs) = x : go (i - 1) xs

drop :: ∀ a. Int -> List a -> List a
drop n = go (max n 0) where
  go :: Int -> List a -> List a
  go _ Nil = Nil
  go 0 l = l
  go i (_ : xs) = drop (i - 1) xs


takeWhile :: ∀ a. (a -> Boolean) -> List a -> List a
takeWhile _ Nil = Nil
takeWhile p (x : xs) = 
  if p x then x : takeWhile p xs else Nil


dropWhile :: ∀ a. (a -> Boolean) -> List a -> List a
dropWhile _ Nil = Nil
dropWhile p l@(x : xs) = --creating FULL list by "l" then with "@" destructuring to "(x : xs)"
  if p x then dropWhile p xs else l


takeEnd :: ∀ a. Int -> List a -> List a
takeEnd n l = snd (go l) where
  go :: List a -> Tuple Int (List a)
  go Nil = Tuple 0 Nil
  go (x : xs) = let tup@(Tuple count acc) = go xs in
    if count < n then Tuple (count + 1) (x : acc)
    else tup

dropEnd :: ∀ a. Int -> List a -> List a
dropEnd n l = snd (go l) where
  go :: List a -> Tuple Int (List a)
  go Nil = Tuple 0 Nil
  go (x : xs) = let Tuple count acc = go xs in
    if count < n then Tuple (count + 1) acc  -- count n より小さい間はdropを続ける
    else Tuple count (x : acc) -- count n　以上になったら accに prepend

foldl :: ∀ a b. (b -> a -> b) -> b -> List a -> b
foldl _ acc Nil = acc
foldl f acc (x : xs) = foldl f (f acc x) xs

reverse' :: ∀ a. List a -> List a
reverse' l = foldl (flip cons) (Nil :: List a) l


foldr' :: ∀ a b. (a -> b -> b) -> b -> List a -> b
foldr' _ acc Nil = acc
foldr' f acc (x : xs) = f x (foldr' f acc xs)

foldr :: ∀ a b. (a -> b -> b) -> b -> List a -> b
foldr f acc l = foldl (flip f) acc (reverse' l)

append :: ∀ a. List a -> List a -> List a
append l1 l2 = foldr cons l2 l1

map :: ∀ a b. (a -> b) -> List a -> List b
map f l = foldr (\x -> \y -> f x : y) (Nil :: List b) l

flatMap :: ∀ a b. (a -> List b) -> List a -> List b
flatMap f l = foldr (\x -> \y -> append (f x) y) (Nil :: List b) l

map2 :: ∀ a b c. (a -> b -> c) -> List a -> List b -> List c
map2 f la lb = 
  flatMap (\xa -> map (\yb -> f xa yb) lb) la

zip :: ∀ a b. List a -> List b -> List (Tuple a b)
-- zip = map2 Tuple
zip Nil _ = Nil
zip _ Nil = Nil
zip (x : xs) (y : ys) = Tuple x y : zip xs ys

-- record -> columner
unzip :: ∀ a b. List (Tuple a b) -> Tuple (List a) (List b)
-- unzip l = Tuple (map fst l) (map snd l)
unzip Nil = Tuple Nil Nil
unzip ((Tuple a b) : xs) = let Tuple as bs = unzip xs in
  Tuple (a : as) (b : bs)

test :: Effect Unit
test = do
  log $ show $ flip const 1 2
  flip const 1 2 # show # log
  log $ show $ singleton "xyz"
  log $ show $ cons 0 (1 : 2 : Nil)
  log $ show $ null Nil
  log $ show $ null ("xyz" : Nil)
  log $ show $ snoc Nil "xyz"
  log $ show $ snoc ("abc" : Nil) "xyz"
  log $ show $ length Nil
  log $ show $ length (1 : 2 : 3 : Nil)
  log $ show $ head (Nil:: List Unit)
  log $ show $ head (1 : 2 : 3 : Nil)
  log $ show $ tail (Nil::List Unit)
  log $ show $ tail ("abc" : "123" : Nil)
  log $ show $ last (Nil:: List Unit)
  log $ show $ last (1 : 2 : 3 : Nil)
  log $ show $ init (Nil:: List Unit)
  log $ show $ init (1 : Nil)
  log $ show $ init (1 : 2 : 3 : Nil)
  log $ show $ uncons (1 : 2 : 3 : Nil)
  log $ show $ index (1 : Nil) 4
  log $ show $ index (1 : 2 : 3 : Nil) 1
  log $ show $ index (Nil :: List Unit) 0
  log $ show $ (1 : 2 : 3 : Nil)!!1
  log $ show $ findIndex (_ >= 2) (1 : 2 : 3 : Nil) -- Just 1
  log $ show $ findIndex (_ >= 99) (1 : 2 : 3 : Nil) -- Nothing
  log $ show $ findIndex (10 /= _) (Nil :: List Int) -- Nothing
  log $ show $ findLastIndex (_ == 10) (Nil :: List Int)
  log $ show $ findLastIndex (_ == 10) (10 : 5 : 10 : -1 : 2 : 10 : Nil) 
  log $ show $ findLastIndex (_ == 10) (11 : 12 : Nil)
  log $ show $ reverse (10 : 20 : 30 : Nil)
  log $ show $ reverse' (10 : 20 : 30 : Nil)
  log $ show $ append (1 : 2 : 3 : Nil) (4 : 5 : 6 : Nil)
  log $ show $
    concat ((1 : 2 : 3 : Nil) : (4 : 5 : Nil) : (6 : Nil) : (Nil) : Nil)
  log $ show $ filter (4 > _) $ (1 : 2 : 3 : 4 : 5 : 6 : Nil)
  log $ show $
    catMaybes (Just 1 : Nothing : Just 2 : Nothing : Nothing : Just 5 : Nil)
  log $ show $
    catMaybes' (Just 1 : Nothing : Just 2 : Nothing : Nothing : Just 5 : Nil)
  log $ show $ range 1 10
  log $ show $ range 3 (-3)

  log $ show $ range' 1 10
  log $ show $ range' 3 (-3)

  log $ show $ range'' 1 10
  log $ show $ range'' 3 (-3)
  log $ show $ take 5 (12 : 13 : 14 : Nil)
  log $ show $ take 5 (-7 : 9 : 0 : 12 : -13 : 45 : 976 : Nil)
  log $ show $ take' 5 (12 : 13 : 14 : Nil)
  log $ show $ take' 5 (-7 : 9 : 0 : 12 : -13 : 45 : 976 : Nil)
  log $ show $ drop 2 (1:2:3:4:5:6:7:Nil)
  log $ show $ drop 10 (Nil :: List Unit)
  log $ show $ takeWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil)
  log $ show $ takeWhile (_ == -17) (1 : 2 : 3 : Nil)
  log $ show $ dropWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil)
  log $ show $ dropWhile (_ == -17) (1 : 2 : 3 : Nil)
  log $ show $ takeEnd 3 (1:2:3:4:5:6:Nil)
  log $ show $ takeEnd 10 (1 : 2 : Nil)

  log $ show $ dropEnd 3 (1:2:3:4:5:6:7:Nil)
  log $ show $ dropEnd 10 (1 : Nil)

  log $ show $ map (_ + 1) (1 : 2 : 3 : Nil)
  log $ show $ flatMap (\x -> x : x : Nil) (1 : 2 : 3 : Nil)
  log $ show $ map2 (\x -> \y -> x + y) (1 : 2 : 3 : Nil) (4 : 5 : 6 : Nil)
  log $ show $ zip (1 : 2 : 3 : Nil) ("a" : "b" : "c" : Nil)
  
  log $ show $ unzip (Tuple 1 "a" : Tuple 2 "b" : Tuple 3 "c" : Nil)
  log $ show $ unzip (Tuple "a" 1 : Tuple "b" 2 : Tuple "c" 3 : Nil)
  log $ show $ unzip (Nil :: List (Tuple Unit Unit))
  





