{-# LANGUAGE DataKinds #-}

module Manual.Types.Unions (examples) where

import Foreign as F

import HsBindgen.Runtime.Union qualified as Union

import Manual.Tools
import Unions
import Unions.Unsafe

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

examples :: IO ()
examples = do
    section "Unions"

    do
      let occupation = Union.set @"occupation_student" Student{
              student_university = nullPtr
            , student_year       = 2000
            }
      print $ Union.get @"occupation_student" occupation
      with occupation $ print_occupation 0

    do
      let occupation = Union.set @"occupation_employee" Employee{
              employee_company    = nullPtr
            , employee_supervisor = nullPtr
            , employee_salary     = 100_000
            }
      print $ Union.get @"occupation_employee" occupation
      with occupation $ print_occupation 1
