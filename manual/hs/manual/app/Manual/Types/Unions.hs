module Manual.Types.Unions (examples) where

import Foreign as F

import Manual.Tools

import Example
import Example.Unsafe

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

examples :: IO ()
examples = do
    section "Unions"

    do let occupation = set_occupation_student Student{
           student_university = nullPtr
         , student_year       = 2000
         }
       with occupation $ print_occupation 0

    do let occupation = set_occupation_employee Employee{
           employee_company    = nullPtr
         , employee_supervisor = nullPtr
         , employee_salary     = 100_000
         }
       with occupation $ print_occupation 1
