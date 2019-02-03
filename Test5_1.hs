module Test5_1 where

import Task5_1

assertEquals :: (Show a, Eq a) => (DList a)-> (DList a) -> String -> String 
assertEquals actual supposed testName= let isOk = (actual == supposed) in
  if isOk then "is OK " ++ testName ++ "\n"
  else ("is FAILED " ++ testName 
    ++ " with params:\nactual = " ++ (show actual)
    ++ "\nsupposed = " ++ (show supposed) ++ "\n")

runTests lst = do 
 putStr $ runTest lst 1
  where 
    runTest [] i = "All tests: " ++ (show $ i - 1) ++ " are finished\n"
    runTest (test:tests) i =  
      "TestCase # "
      ++ (show i)
      ++ " " ++ test 
      ++ (runTest tests (i + 1))

runInsert = runTests insertAtTests
runRemove = runTests removeAtTests
runAll    = runTests allTests

insertAtTests = [
                insertAtTest01,
                insertAtTest02,
                insertAtTest03,
                insertAtTest04,
                insertAtTest05,
                insertAtTest06,
                insertAtTest07,
                insertAtTest08,
                insertAtTest09,
                insertAtTest10,
                insertAtTest11 
                ]
                
removeAtTests = --[]
               [
                removeAtTest01,
                removeAtTest02,
                removeAtTest03,
                removeAtTest04,
                removeAtTest05,
                removeAtTest06,
                removeAtTest07,
                removeAtTest08,
                removeAtTest09,
                removeAtTest10,
                removeAtTest11
                ]
                
allTests = insertAtTests ++ removeAtTests 

nShiftsLeft 0 dl            = dl
nShiftsLeft i DNil          = error $ "left error i = " ++ (show i)
nShiftsLeft i (DCons l v r) = nShiftsLeft (i - 1) l

nShiftsRight 0 dl            = dl
nShiftsRight i DNil          = error $ "right error i = " ++ (show i)
nShiftsRight i (DCons l v r) = nShiftsRight (i - 1) r


insertAtTest01 = assertEquals actual supposed "Insert to begining" 
  where
    inner    = list2dlist [1..3]
    actual   = insertAt inner 0 777 
    supposed = list2dlist $ [777] ++ [1..3]

insertAtTest02 = assertEquals actual supposed "Insert to end" 
  where
    inner    = list2dlist [1..3]
    actual   = insertAt inner 3 777 
    supposed = list2dlist $ [1..3] ++ [777] 

insertAtTest03 = assertEquals actual supposed "Insert to empty" 
  where
    inner    = list2dlist []
    actual   = insertAt inner 0 777 
    supposed = list2dlist [777] 

insertAtTest04 = assertEquals actual supposed "Insert to one element" 
  where
    inner    = list2dlist [1]
    actual   = insertAt inner 0 777 
    supposed = list2dlist [777,1] 

insertAtTest05 = assertEquals actual supposed "Insert to middle of the list" 
  where
    inner    = list2dlist [1..3]
    actual   = insertAt inner 1 777 
    supposed = list2dlist [1,777,2,3] 

insertAtTest06 = assertEquals actual supposed "Check right list after insert" 
  where
    inner    = list2dlist [1..3]
    actual   = right $ insertAt inner 1 777 
    supposed = list2dlist [777,2,3] 

insertAtTest07 = assertEquals actual supposed "Check left right list after insert" 
  where
    inner    = list2dlist [1..3]
    actual   = left $ right $ insertAt inner 1 777 
    supposed = list2dlist [1,777,2,3] 

insertAtTest08 = assertEquals actual supposed "Check left left right right list after insert" 
  where
    inner    = list2dlist [1..3]
    actual   = left $ left $ right $ right $ insertAt inner 1 777 
    supposed = list2dlist [1,777,2,3] 

insertAtTest09 = assertEquals actual supposed "Check n right and n lefts" 
  where
    lim      = 30
    n        = lim - 2
    inner    = list2dlist [1..lim]
    inserted = insertAt inner 1 777 
 
    actual   = nShiftsLeft n (nShiftsRight n inserted) 
    supposed = list2dlist $ [1,777] ++ [2..lim] 

insertAtTest10 = assertEquals actual supposed "Check left equals DNil after insert" 
  where
    inner    = list2dlist [1..3]
    actual   = left $ insertAt inner 1 777 
    supposed = DNil 

insertAtTest11 = assertEquals actual supposed "N shifts left equlas DNil"
  where
    n        = 30 
    inner    = list2dlist [1..n]
    actual   = nShiftsRight (n + 1) (insertAt inner (n - 1) 777) 
    supposed = DNil 


-- remove testcases

removeAtTest01 = assertEquals actual supposed "Remove from begining" 
  where
    inner    = list2dlist [1..3]
    actual   = removeAt inner 0 
    supposed = list2dlist [2,3]

removeAtTest02 = assertEquals actual supposed "Remove from end" 
  where
    inner    = list2dlist [1..3]
    actual   = removeAt inner 2 
    supposed = list2dlist [1,2]  

removeAtTest03 = assertEquals actual supposed "Remove from one element list" 
  where
    inner    = list2dlist [1]
    actual   = removeAt inner 0  
    supposed = DNil 

removeAtTest04 = assertEquals actual supposed "Remove from middle of the list" 
  where
    inner    = list2dlist [1,777,2]
    actual   = removeAt inner 1  
    supposed = list2dlist [1,2] 

removeAtTest05 = assertEquals actual supposed "Check right list after remove" 
  where
    inner    = list2dlist [1,777,2,3]
    actual   = right $ removeAt inner 1 
    supposed = list2dlist [2,3] 

removeAtTest06 = assertEquals actual supposed "Check left right list after remove" 
  where
    inner    = list2dlist [1,777,2,3]
    actual   = left $ right $ removeAt inner 1 
    supposed = list2dlist [1,2,3] 

removeAtTest07 = assertEquals actual supposed "Check left left right right list after remove" 
  where
    inner    = list2dlist [1,2,777,3]
    actual   = left $ left $ right $ right $ removeAt inner 2 
    supposed = list2dlist [1,2,3] 

removeAtTest08 = assertEquals actual supposed "Check left left right right list after remove" 
  where
    inner    = list2dlist [1,2,777,3]
    actual   = left $ left $ right $ right $ removeAt inner 2  
    supposed = list2dlist [1,2,3] 

removeAtTest09 = assertEquals actual supposed "Check n right and n lefts" 
  where
    lim      = 30
    n        = lim - 2
    inner    = list2dlist [1..lim]
    removed = removeAt inner 4  
    actual   = nShiftsLeft (n - 1) (nShiftsRight (n - 1) removed) 
    supposed = list2dlist $ [1..4] ++ [6..lim] 

removeAtTest10 = assertEquals actual supposed "Check left equals DNil after remove" 
  where
    inner    = list2dlist [1,2,777,3]
    actual   = left $ removeAt inner 2 
    supposed = DNil 

removeAtTest11 = assertEquals actual supposed "N shifts left equlas DNil"
  where
    n        = 30 
    inner    = list2dlist [1..n]
    actual   = nShiftsRight (n - 1) (removeAt inner (n - 1)) 
    supposed = DNil 
