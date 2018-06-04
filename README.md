# The LETREC Language

A Haskell implementation of the LETREC language specification from the book *Essentials of Programming Languages*, 3rd edition, by Friedman and Wand. This is only intended as a study project and borrows most of its code from [my implementation of the PROC language](https://github.com/groscoe/friedman-wand-proc-language). A reference implementation in Racket is available on [Github](https://github.com/mwand/eopl3).

This language accepts all expressions accepted by PROC, plus functions can be recursive. Contrary to the specification in the book, I chose to extend the evaluation of normal function application to be recursive rather than adding a new constructor.

## Interesting things the language can do

  - Common recursive functions: 
  ```letproc fac(x) =
      if equal?(x, 0)
      then 1
      else *(x, fac(-(x, 1)))
     in fac(5)```
