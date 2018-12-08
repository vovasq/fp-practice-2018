module Task3_2 where

import Todo(todo)

data ReverseList a = RNil | RCons (ReverseList a) a

rlistToList :: ReverseList a -> [a]
rlistToList RNil            = []
rlistToList (RCons lst1 l1) = l1 : rlistToList lst1


listToRList :: [a] -> ReverseList a
listToRList lst = foldl RCons RNil lst

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor
instance (Eq a) => Eq (ReverseList a) where
  (==) RNil RNil = True
  (==) (RCons lst1 l1) (RCons lst2 l2) = 
    if l1 == l2 then (==) lst1 lst2 else False  

instance (Show a) => Show (ReverseList a) where
  show RNil = "RNil"
  show (RCons RNil l) = show l
  show (RCons lst l)  = show lst ++ ", " ++ show l 

instance (Ord a) =>  Ord (ReverseList a) where
  (<=) RNil _ = False
  (<=) _ RNil = True
  (<=) (RCons lst1 l1) (RCons lst2 l2) | l1 == l2  = lst1 <= lst2 
                                       | l1 < l2   = True
                                       | otherwise = False

instance Semigroup (ReverseList a) where
    (<>) lst1 RNil = lst1
    (<>) lst1 (RCons lst2 l) = RCons (lst1 <> lst2) l

instance Monoid (ReverseList a) where
    mempty = RNil

instance Functor ReverseList where
    fmap _ RNil = RNil
    fmap f (RCons lst l) = RCons (fmap f lst) (f l)

