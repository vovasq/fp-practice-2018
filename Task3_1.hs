module Task3_1 where

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

-- Реализуйте все классы типов, которым должны отвечать целые числа
-- http://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#t:Integer

instance Eq WeirdPeanoNumber where
  (==) Zero Zero = True
  (==) w1 w2 = (wpnToInteger w1) == (wpnToInteger w2)


normalize :: WeirdPeanoNumber -> WeirdPeanoNumber
normalize w1 = normByInt Zero intNumber where
  intNumber = wpnToInteger w1
  normByInt w1 intNum 
    | intNum > 0 = normByInt (Succ w1) (intNum - 1)
    | intNum < 0 = normByInt (Pred w1) (intNum + 1)
    | otherwise   = w1

instance Show WeirdPeanoNumber where
  show x = showNorm (normalize x) where 
    showNorm Zero = "Zero"
    showNorm (Pred x) = "Pred " ++ (showNorm x)
    showNorm (Succ x) = "Succ " ++ (showNorm x)
       
      
wpnToInteger :: WeirdPeanoNumber -> Integer
wpnToInteger (Zero)   = 0
wpnToInteger (Pred x) = wpnToInteger x - 1
wpnToInteger (Succ x) = wpnToInteger x + 1

wpnPlus :: WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber
wpnPlus Zero w2      = w2
wpnPlus (Succ w1) w2 = Succ (w2 + w1)
wpnPlus (Pred w1) w2 = Pred (w2 + w1)

wpnNegate :: WeirdPeanoNumber -> WeirdPeanoNumber
wpnNegate Zero = Zero
wpnNegate (Pred w1) = Succ (wpnNegate w1)  
wpnNegate (Succ w1) = Pred (wpnNegate w1)  

wpnAbs :: WeirdPeanoNumber -> WeirdPeanoNumber
wpnAbs w1 = if w1 > Zero then w1 else wpnNegate w1

wpnSignum :: WeirdPeanoNumber -> WeirdPeanoNumber 
wpnSignum Zero      = Zero
wpnSignum (Succ _)  = Succ Zero
wpnSignum (Pred _)  = Pred Zero

wpnProd :: WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber
wpnProd Zero _ = Zero
wpnProd _ Zero = Zero
wpnProd w1 w2  = let  
  nPlus w 0 = Zero 
  nPlus w n = wpnPlus w (nPlus w (n - 1))
  absProd w1 w2 =  nPlus (wpnAbs w1) (wpnToInteger (wpnAbs w2)) in
    if wpnSignum w1 /= wpnSignum w2  then wpnNegate (absProd w1 w2) else (absProd w1 w2)

wpnFromInteger :: Integer -> WeirdPeanoNumber
wpnFromInteger i | i == 0 = Zero
                 | i > 0  = Succ  (wpnFromInteger (i - 1))
                 | i < 0  = Pred  (wpnFromInteger (i + 1))

wpnQuotRem :: WeirdPeanoNumber -> WeirdPeanoNumber -> (WeirdPeanoNumber, WeirdPeanoNumber)
wpnQuotRem _ Zero = error "Divide by Zero exception"
wpnQuotRem Zero _ = (Zero, Zero) 
wpnQuotRem w1 w2  = if wpnSignum w1 /= wpnSignum w2  then (wpnNegate (fst (absDiv w1 w2 0)), wpnNegate (snd (absDiv w1 w2 0)))  else absDiv w1 w2 0 where
  absDiv w1 w2 n = recurs (wpnAbs w1) (wpnAbs w2) n where 
    recurs w1 w2 n = if w1 >= w2 then recurs (w1 - w2) w2 (n + 1)
        else if w1 == Zero then (wpnFromInteger n, Zero) else (wpnFromInteger n, w1)

instance Integral WeirdPeanoNumber where
    toInteger = wpnToInteger
    quotRem   = wpnQuotRem 

instance Num WeirdPeanoNumber where
-- Minimal complete definition
-- (+) , (*), abs, signum, fromInteger, (negate | (-))
  (+)         = wpnPlus 
  (*)         = wpnProd
  negate      = wpnNegate
  abs         = wpnAbs
  signum      = wpnSignum
  fromInteger = wpnFromInteger

wpnToEnum :: Int -> WeirdPeanoNumber
wpnToEnum i | i == 0 = Zero
            | i > 0  = Succ (wpnToEnum (i - 1))
            | i < 0  = Pred (wpnToEnum (i + 1))

wpnFromEnum :: WeirdPeanoNumber -> Int
wpnFromEnum Zero = 0
wpnFromEnum (Succ prev) = wpnFromEnum prev + 1 
wpnFromEnum (Pred prev) = wpnFromEnum prev - 1

instance Enum WeirdPeanoNumber where
  fromEnum  = wpnFromEnum
  toEnum    = wpnToEnum 

instance Ord WeirdPeanoNumber where
    (<=) x y = (wpnToInteger x) <= (wpnToInteger y) 

instance Real WeirdPeanoNumber where
  toRational w = toRational (wpnToInteger w)
