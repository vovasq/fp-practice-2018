module Task3_3 where

newtype PSet a = PSet{ contains :: (a -> Bool) }

-- Реализуйте классы Monoid и Functor
-- Объясните в комментариях, почему они реализованы именно так


-- Объединение множеств
newtype UnionPSet a = UnionPSet { containsUnion :: (a -> Bool) }

instance Semigroup (UnionPSet  a) where
  (<>) (UnionPSet p1) (UnionPSet  p2) = UnionPSet  (\x -> p1 x || p2 x)

instance Monoid (UnionPSet  a) where
  mempty = UnionPSet (\x -> False)

-- Пересечение множеств
newtype IntersectPSet a = IntersectPSet{ containsIntersect :: (a -> Bool) }

instance Semigroup (IntersectPSet a) where
  (<>) (IntersectPSet p1) (IntersectPSet p2) = IntersectPSet (\x -> p1 x && p2 x)

instance Monoid (IntersectPSet a) where
  mempty = IntersectPSet (\x -> False)

-- Симметричная разность множеств
newtype DiffPSet a = DiffPSet{ containsDiff :: (a -> Bool) }

instance Semigroup (DiffPSet a) where
  (<>) (DiffPSet p1) (DiffPSet p2) = DiffPSet (\x -> (p1 x || p2 x) && not (p1 x && p2 x))
        
instance Monoid (DiffPSet a) where
  mempty = DiffPSet (\x -> False)

-- тут идея в следующем: 
-- знаем только отображение из множества A в B
-- нельзя сделать никаких выводов о множестве B поэтому всегда false
instance Functor PSet where
  fmap f (PSet fa) = PSet (\b -> False)


