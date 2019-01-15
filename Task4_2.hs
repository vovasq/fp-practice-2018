module Task4_2 where

data FourOf a = FourOf a a a a deriving(Show,Eq)

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FourOf`
-- таким образом, что 
check =  (do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y } == FourOf 5 8 10 12)
--  
-- http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html


instance Functor FourOf where
  fmap f (FourOf a b c d) = FourOf (f a) (f b) (f c) (f d)  

instance Applicative FourOf where
  pure a =  (FourOf a a a a)
  (<*>) (FourOf f1 f2 f3 f4) (FourOf a b c d) = FourOf (f1 a) (f2 b) (f3 c) (f4 d)   
 
instance Monad FourOf where
    return a  = (FourOf a a a a) 
    (>>=) (FourOf a b c d) f = FourOf (f1(f a)) (f2(f b)) (f3(f c)) (f4(f d)) where
      f1 (FourOf a _ _ _) = a
      f2 (FourOf _ a _ _) = a
      f3 (FourOf _ _ a _) = a
      f4 (FourOf _ _ _ a) = a
