# The LETREC Language

A Haskell implementation of the LETREC language specification from the book *Essentials of Programming Languages*, 3rd edition, by Friedman and Wand. This is only intended as a study project and borrows most of its code from [my implementation of the PROC language](https://github.com/groscoe/friedman-wand-proc-language). A reference implementation in Racket is available on [Github](https://github.com/mwand/eopl3).

This language accepts all expressions accepted by PROC, plus functions can be recursive. Contrary to the specification in the book, I chose to extend the evaluation of normal function application to be recursive rather than adding a new constructor.

## Interesting things the language can do

  - Common recursive functions: 
  ```
  letproc factorial(x) =
   if equal?(x, 0)
   then 1
   else *(x, factorial(-(x, 1)))
  in factorial(5)
  ```
     
  - Higher order recursive functions:
  ```
  letproc map(fun, xs) = 
    if null?(xs)
    then emptylist
    else let head = car(xs)
             tail = cdr(xs)
         in cons(fun(head), map(fun, tail))
  in let addOne = proc(x) +(x, 1)
         xs = list(1,2,3)
     in map(addOne, xs)
  ```
  
  ```
  letproc
  
    fold(f, e, xs) =
      if null?(xs)
      then e
      else let head = car(xs)
               tail = cdr(xs)
           in f(head, fold(f, e, tail))
           
    sum(xs) = fold(proc(x,y) +(x,y), 0, xs)
    
  in sum(list(1,2,3,4,5))"
  ```

  - Mutually recursive functions:
  ```
  letproc
    even(x) = if zero?(x) then 1 else odd(-(x,1))
    odd(x) = if zero?(x) then 0 else even(-(x,1))
  in odd(13)
  ```
