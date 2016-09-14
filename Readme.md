Free Monads Talk
================

See [FreeMonads.lhs](FreeMonads.lhs) for the presentation materials.

Abstract
--------

This talk demonstrates a domain specific language (DSL) implemented using the
Free Monad. We will write a small language for performing operations on the
filesystem, and explore the benefits and drawbacks that come from this approach
to structuring programs.

Setup
-----

To get started, clone this repo and run GHCi.

``` sh
$ git clone https://github.com/chrisbarrett/free-monads-talk.git
$ cd free-monads-talk
$ stack ghci
```

Once inside GHCi, you can load the Haskell source file and run the example code.

    λ> :l FreeMonads
    λ> example1
    λ> example2
