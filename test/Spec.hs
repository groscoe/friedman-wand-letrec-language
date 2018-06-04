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

ex2 :: String
ex2 = "letproc\n\
      \  even(x) = if zero?(x) then 1 else odd(-(x,1))\n\
      \  odd(x) = if zero?(x) then 0 else even(-(x,1))\n\
      \in odd(13)"

ex3 :: String
ex3 = "letproc\n\
      \  fold(f, e, xs) =\n\
      \    if null?(xs)\n\
      \    then e\n\
      \    else let head = car(xs)\n\
      \             tail = cdr(xs)\n\
      \         in f(head, fold(f, e, tail))\n\n\
      \  sum(xs) = fold(proc(x,y) +(x,y), 0, xs)\n\n\
      \in sum(list(1,2,3,4,5))"
