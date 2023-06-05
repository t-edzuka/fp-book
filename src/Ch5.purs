module Ch5 (test)
  where

import Data.List (List(Nil, Cons), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude (type (~>), Unit, discard, negate, otherwise, show, (+), (-), (/=), (<), (<<<), (==), (>), (>=))
import Prim.RowList (Nil)


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

-- unitなどとも呼ばれている
singleton :: ∀ a. a -> List a
singleton x = x : Nil

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

append :: ∀ a. List a -> List a -> List a
append Nil rl = rl
append (x : xs) rl = x : append xs rl

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


test :: Effect Unit
test = do
  log $ show $ flip const 1 2
  flip const 1 2 # show # log
  log $ show $ singleton "xyz"
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



