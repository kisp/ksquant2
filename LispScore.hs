module LispScore where

import qualified AbstractScore as A
import Lisp

type Score = A.Score LispVal
type Part = A.Part LispVal
type Voice = A.Voice LispVal
