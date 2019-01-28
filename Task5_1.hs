module Task5_1 where

import Todo(todo)

data DList a = DNil 
             | DCons { 
                left    :: (DList a), 
                current :: a, 
                right   :: (DList a) 
             }

instance (Show a) => Show (DList a) where
    show it = "Dlist [" ++ showBody it ++ "]"
              where showBody DNil = ""
                    showBody (DCons _ h DNil) = show h
                    showBody (DCons _ h t) = show h ++ ", " ++ showBody t

instance (Eq a) => Eq (DList a) where
    DNil == DNil = True
    (DCons _ h1 t1) == (DCons _ h2 t2) = h1 == h2 && t1 == t2
    _ == _ = False


list2dlist :: [a] -> DList a
list2dlist lst = list2dlist' DNil lst 
  where 
    -- list2dlist' :: DList a -> [a] -> DList a
        list2dlist' _ [] = DNil
        list2dlist' left (h: t) = 
         let rec = DCons left h (list2dlist' rec t)
         in rec 

index :: DList a -> Int -> a
index dlst i = if i < 0 then error "index less zero" 
  else getIndex i dlst 0 where 
    getIndex i DNil cnt          = if i >= cnt then error "index out of bound" 
      else error "kek" 
    getIndex i (DCons _ h t) cnt =  
      if cnt == i then h else getIndex i t (cnt + 1)       

sizeD :: DList a -> Int 
sizeD dlst = sizeCounter 0 dlst
  where sizeCounter cnt DNil = cnt
        sizeCounter cnt (DCons _ h t) = sizeCounter (cnt + 1) t 

checkIfWrongIndex :: DList a -> Int -> Bool
checkIfWrongIndex dlst index | (sizeD dlst <= index || index < 0) =  True
                             | otherwise = False

updateLeft DNil _ = DNil
updateLeft (DCons l c r) left = 
    let new = DCons left c (updateLeft r new)
    in new


insertAt :: DList a -> Int -> a -> DList a
insertAt dlst index value = 
  if checkIfWrongIndex dlst index then error "wrong index"
  else inserter  dlst index value where
    inserter DNil 0 v          = DCons DNil v DNil
    -- inserter (DCons l h r) 0 v = updateLeft (DCons l v (DCons DNil h r)) (DCons DNil h r)  
    inserter (DCons l h r) 0 v = DCons l v (DCons DNil h r) 
    inserter (DCons l h r) i v = updateLeft (DCons l h (inserter r (i - 1) v)) (inserter r (i - 1) v)   
    -- inserter (DCons l h r) i v = DCons l h (inserter r (i - 1) v)



-- [1, 2]
-- dnil 1 (dcons (dnil 1...) )
  
-- [1,2] -> DCons DNil 1 ((DNil) 2 DNil)
-- [1,2,3]

-- 1 2 3 4 5 6 7 8 9 10

removeAt :: DList a -> Int -> DList a
removeAt dlst index = 
  if checkIfWrongIndex dlst index then error "wrong index"
  else remover dlst index where 
    -- remover (DCons (DCons _ _ _ ) _ _ ) _ = error "pattern error for real" 
    remover (DCons _ v DNil) 0                 = DNil   
    remover (DCons l v (DCons _ rv r)) 0    = DCons l rv r 
    -- remover (DCons DNil v (DCons DNil rv DNil)) 0 = DCons DNil rv DNil 
    remover (DCons l v r) cnt                  = updateLeft (DCons l v (remover r (cnt - 1))) (DCons l v (remover r (cnt - 1)))   


dls   = list2dlist [1..5]
dls6  = insertAt dls 1 777
check = left (right dls6)
