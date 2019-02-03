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
sizeD dlst = sizeCounter 1 dlst
  where sizeCounter cnt DNil = cnt 
        sizeCounter cnt (DCons _ h t) = sizeCounter (cnt + 1) t 

checkIfWrongIndex :: DList a -> Int -> Bool
checkIfWrongIndex dlst index | (sizeD dlst <= index || index < 0) =  True
                             | otherwise = False
   
updateLeft :: DList a -> DList a -> DList a
updateLeft DNil          _    = DNil
updateLeft (DCons _ v r) left = res
 where
  res = DCons left v (updateLeft r res)

insertAt :: DList a -> Int -> a -> DList a
insertAt dlst index value = 
  if checkIfWrongIndex dlst index then error "wrong index"
  else inserter DNil dlst index value where
    inserter left DNil  0 value = DCons left value DNil
    inserter left right 0 value = res
      where res = DCons left value (updateLeft right res)
    inserter left (DCons _ v r) index value = res
      where res = DCons left v (inserter res r (index - 1) value)

removeAt :: DList a -> Int -> DList a
removeAt dlst index = 
  if checkIfWrongIndex dlst index then error "wrong index"
  else remover DNil dlst index where
    remover _ DNil _                     = error "wrong index"
    remover left (DCons _ _ right) 0     = updateLeft right left
    remover left (DCons _ v right) index = res
      where
        res = DCons left v (remover res right (index - 1))