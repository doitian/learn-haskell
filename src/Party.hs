module Party (maxFun) where

import Data.Tree
import Employee

-- | Add an employee to the guest list.
--
-- It's assumed that both the new employee and his/her direct boss are not in
-- the list.
--
-- ==== Examples
-- >>> glCons (Emp "Stan" 9) (GL [(Emp "Joe" 5)] 5)
-- GL [Emp {empName = "Stan", empFun = 9},Emp {empName = "Joe", empFun = 5}] 14
glCons :: Employee -> GuestList -> GuestList
glCons e (GL es fun) = GL (e : es) (empFun e + fun)

-- |
-- >>> moreFun (GL [] 9) (GL [] 5)
-- GL [] 9
-- >>> moreFun (GL [] 5) (GL [] 9)
-- GL [] 9
moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss divisionLists = (withBoss, withoutBoss)
  where
    -- Invite boss, thus exclude direct subordinates
    withBoss = glCons boss . mconcat . map snd $ divisionLists
    -- Exclude boss, merge best from divisions
    withoutBoss = mconcat . map (uncurry moreFun) $ divisionLists

-- |
-- >>> let (GL _ f) = maxFun testCompany2 in f
-- 26
-- >>> let (GL _ f) = maxFun (subForest testCompany2 !! 0) in f
-- 9
-- >>> let (GL _ f) = maxFun (subForest testCompany2 !! 1) in f
-- 17
maxFun :: Tree Employee -> GuestList
maxFun = uncurry max . eitherMax
  where
    -- Given a Tree, returns @(with, without)@
    eitherMax (Node boss forests) = nextLevel boss $ map eitherMax forests
