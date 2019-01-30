module Task5_1 where

import Todo(todo)
import Control.Exception


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
   

insertAt :: DList a -> Int -> a -> DList a
insertAt dlst index value = 
  if checkIfWrongIndex dlst index then error "wrong index"
  else inserter dlst index value where
    inserter DNil 0 v          = DCons DNil v DNil
    inserter (DCons l h r) 0 v = DCons l v (DCons DNil h r) 
    inserter (DCons l h r) i v = DCons l h (inserter r (i - 1) v)  

removeAt :: DList a -> Int -> DList a
removeAt dlst index = 
  if checkIfWrongIndex dlst index then error "wrong index"
  else remover dlst index where 
    -- remover (DCons (DCons _ _ _ ) _ _ ) _ = error "pattern error for real" 
    remover (DCons _ v DNil) 0                 = DNil   
    remover (DCons l v (DCons _ rv r)) 0    = DCons l rv r 
    -- remover (DCons DNil v (DCons DNil rv DNil)) 0 = DCons DNil rv DNil 
    remover (DCons l v r) cnt                  = DCons l v (remover r (cnt - 1)) 




assertEquals :: (Show a, Eq a) => (DList a)-> (DList a) -> String -> String 
assertEquals actual supposed testName= let isOk = (actual == supposed) in
  if isOk then "is OK " ++ testName ++ "\n"
  else ("is FAILED " ++ testName 
    ++ " with params:\nactual = " ++ (show actual)
    ++ "\nsupposed = " ++ (show supposed) ++ "\n")

dls   = list2dlist [1..5]
dls6  = insertAt dls 1 777
check = left (right dls6)


-- -- insert to begining
-- 1-2-3 insertAt 0 77 77-1-2-3

test1 = assertEquals actual supposed "Insert to begining" 
  where
    inner    = list2dlist [1..3]
    actual   = insertAt inner 0 777 
    supposed = list2dlist $ [777] ++ [1..3]

test2 = assertEquals actual supposed "Insert to end" 
  where
    inner    = list2dlist [1..3]
    actual   = insertAt inner 2 777 
    supposed = list2dlist $ [1..3] ++ [777] 


test3 = assertEquals actual supposed "Insert to empty" 
  where
    inner    = list2dlist []
    actual   = insertAt inner 0 777 
    supposed = list2dlist [777] 

test4 = assertEquals actual supposed "Insert to one element list" 
  where
    inner    = list2dlist [1]
    actual   = insertAt inner 0 777 
    supposed = list2dlist [1,777] 


allTestCases = [test1,
                test2,
                test3,
                test4] 


runAllTests = do 
 putStr $ runTest allTestCases 1
  where 
    runTest [] i = "All tests: " ++ (show $ i - 1) ++ " are finished\n"
    runTest (test:tests) i =  
      "TestCase # "
      ++ (show i)
      ++ " " ++ test 
      ++ (runTest tests (i + 1))


-- -- insert to begining
-- 1-2-3 insertAt 0 77 77-1-2-3

-- -- insert to middle
-- 1-2 insertAt 1 77  1-77-2 

-- -- insert to back 
-- 1-2-3 insertAt 2 77 1-2-3-77r



-- -- wrong insert index -> less zero
-- 1-2-3 insertAt 3 77 "error"

-- -- wrong insert index -> less zero
-- 1-2-3 insertAt -1 "error"