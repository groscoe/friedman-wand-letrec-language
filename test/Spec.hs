module Test where

import Eval

ex0 :: String
ex0 = "letproc factorial(x) =\n\
      \  if equal?(x, 0)\n\
      \  then 1\n\
      \  else *(x, factorial(-(x, 1)))\n\
      \in factorial(5)"

ex1 :: String
ex1 = "letproc map(fun, xs) = \n\
      \  if null?(xs)\n\
      \  then emptylist\n\
      \  else let head = car(xs)\n\
      \           tail = cdr(xs)\n\
      \       in cons(fun(head), map(fun, tail))\n\
      \in let f = proc(x) +(x, 1)\n\
      \       xs = list(1,2,3)\n\
      \   in map(f, xs)"
